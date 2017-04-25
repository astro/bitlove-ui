{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeUIFoundation
    ) where

import Import hiding (loadConfig)
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getFaviconR)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Data.Pool
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types.Status
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Char8 as BC
--import Data.Monoid
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip
import Network.HTTP.Types


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Auth
import Handler.Browse
import Handler.Directory
import Handler.Edit
import Handler.TorrentFile
import Handler.TorrentStats
import Handler.MapFeed
import Handler.DownloadFeeds
import Handler.ByEnclosureAPI
import Handler.FeedLookupAPI
import Handler.Tracker (makeTrackerApp)
import Handler.Help
import Handler.Widget
import Handler.Thumbnails
import Stats


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
-- TODO: remove -fno-warn-name-shadowing
mkYesodDispatch "UIApp" resourcesUIApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig BitloveEnv Extra -> IO Application
makeApplication conf = do
    dbconf <- withYamlEnvironment
              "config/postgresql.yml"
              (appEnv conf)
              parseDBConf
    uiPool <- makeDBPool dbconf
    trackerPool <- makeDBPool dbconf
    foundation <- makeUIFoundation conf uiPool
    tracker <- ignoreAccept <$>
               (makeTrackerApp trackerPool >>= toWaiAppPlain)
    stats <- statsMiddleware (appEnv conf) trackerPool
    ui <- enforceVhost . stats . gzip def . autohead . etagMiddleware . ignoreAccept <$>
          toWaiAppPlain foundation
    return $ measureDuration $ anyApp [tracker, ui]
  where
    anyApp [app] =
        app
    anyApp (app:apps) =
        \req respond ->
            app req $ \res ->
                let tryNext =
                        case res of
                          ResponseFile (Status 404 _) _ _ _ ->
                              True
                          ResponseBuilder (Status 404 _) _ _ ->
                              True
                          ResponseStream (Status 404 _) _ _ ->
                              True
                          _ ->
                              False
                in if tryNext
                   then anyApp apps req respond
                   else respond res
    anyApp [] =
        \_req respond ->
            respond $ ResponseBuilder (Status 404 "Not found") [] mempty
    enforceVhost :: Middleware
    enforceVhost app req respond =
        let proceed = app req respond
            getRedirectResponse location = respond $
                                ResponseBuilder (Status 301 "Wrong vhost")
                                [("Location", location)]
                                mempty
        in case "Host" `lookup` requestHeaders req of
             Nothing ->
                 proceed
             Just vhost
                 | any (`BC.isPrefixOf` vhost) ["localhost", "bitlove.org", "api.bitlove.org"] ->
                     proceed
             Just _ ->
                 getRedirectResponse $
                 "https://bitlove.org" `BC.append` rawPathInfo req
    measureDuration :: Middleware
    measureDuration app req respond =
        do cpu1 <- getCPUTime
           utc1 <- getCurrentTime
           app req $ \res ->
               do cpu2 <- res `seq`
                          getCPUTime
                  utc2 <- getCurrentTime

                  a <- respond res

                  cpu3 <- getCPUTime
                  utc3 <- getCurrentTime

                  let remote = show $ remoteHost req
                      proxied = BC.unpack <$>
                                "X-Real-IP" `lookup` requestHeaders req
                      remote' = maybe remote ((remote ++ "/") ++) proxied
                  BC.hPutStrLn stderr $ BC.concat
                             [ "["
                             , BC.pack $ show (truncate $ (utc2 `diffUTCTime` utc1) * 1000 :: Int)
                             , "+"
                             , BC.pack $ show (truncate $ (utc3 `diffUTCTime` utc2) * 1000 :: Int)
                             , "ms real "
                             , BC.pack $ show ((cpu2 - cpu1) `div` 1000000000)
                             , "+"
                             , BC.pack $ show ((cpu3 - cpu2) `div` 1000000000)
                             , "ms cpu] "
                             , BC.pack remote'
                             , " "
                             , BC.pack $ show $ statusCode $ responseStatus res
                             , " "
                             , requestMethod req
                             , " "
                             , rawPathInfo req
                             ]

                  return a

    etagMiddleware app req respond =
      do let reqETags =
                 "If-None-Match" `lookup` requestHeaders req
             matches etag =
                 case (etag `BC.breakSubstring`) <$> reqETags of
                   Just (_, reqEtags')
                       | not (BC.null reqEtags') ->
                           True
                   _ ->
                       False
         app req $ \res ->
             do let may304 res s hs
                        | statusCode s == 200 &&
                          maybe False matches ("ETag" `lookup` hs) =
                              ResponseBuilder notModified304 hs mempty
                        | otherwise =
                            res
                    res' = case res of
                             ResponseFile s hs _ _ ->
                                 may304 res s hs
                             ResponseBuilder s hs _ ->
                                 may304 res s hs
                             ResponseStream s hs _ ->
                                 may304 res s hs
                             raw -> raw
                respond res'

    -- | Ignore the "Accept:" header because Yesod is very strict
    -- about typed content choice.
    ignoreAccept :: Middleware
    ignoreAccept app req = app req'
        where requestHeaders' = requestHeaders req
              removeHeader :: HeaderName -> RequestHeaders -> RequestHeaders
              removeHeader name = filter ((name /=) . fst)
              requestHeaders'' = removeHeader hAccept requestHeaders'
              req' = req { requestHeaders = requestHeaders'' }

getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Settings.Development)
        { csParseExtra = parseExtra
        }

makeUIFoundation :: AppConfig BitloveEnv Extra -> DBPool -> IO UIApp
makeUIFoundation conf pool = do
    manager <- newManager tlsManagerSettings
    s <- staticSite
    return $ UIApp conf s pool manager

parseDBConf :: Monad m =>
               Value -> m [(String, String)]
parseDBConf = return . parse
  where parse (Aeson.Object o) = do
          (k, v) <- HashMap.toList o
          let k' = T.unpack k
          case v of
            Aeson.String v' ->
              return (k', T.unpack v')
            (Aeson.Object _) ->
              parse v
            (Aeson.Number n) ->
              return (k', show n)
            _ ->
              error ("Cannot parse: " ++ show v)
        parse _ = error "Expected JSON object"

connectDB :: [(String, String)] -> IO PQ.Connection
connectDB dbconf = do
  putStrLn $ "PQ.connect " ++ show dbconf
  db <- PQ.connectdb $ encodeUtf8 conf
  mMsg <- PQ.errorMessage db
  case mMsg of
    Just msg | not (BC.null msg) -> do
      let msg' = T.unpack (decodeUtf8 msg)
      putStrLn $ "Error: " ++ msg'
      PQ.finish db
      error msg'
    _ -> return db
  where pqKeys = ["user", "password", "host", "port", "dbname", "hostaddr"]
        dbconf' =
          filter ((`elem` pqKeys) . fst) dbconf
        conf = T.intercalate " " $ do
          (k, v) <- dbconf'
          return $ T.concat [T.pack k, "=", T.pack v]

makeDBPool :: [(String, String)] -> IO DBPool
makeDBPool dbconf =
  let connect = connectDB dbconf
  in createPool
     (hPutStrLn stderr "PQ connect" >> connect)
     (\db -> hPutStrLn stderr "PQ disconnect" >> PQ.finish db)
     4 60 4


getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "static/favicon.ico"
