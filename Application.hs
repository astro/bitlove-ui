{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeUIFoundation
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getFaviconR)
import Network.HTTP.Conduit (newManager, def)
import Data.Conduit.Pool
import Database.HDBC as HDBC (disconnect)
import Database.HDBC.PostgreSQL
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types.Status
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Char8 as BC
--import Data.Monoid
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Autohead
import Control.Monad.Trans.Resource (register)


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
import Handler.Tracker
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
    tracker <- makeTrackerApp trackerPool >>= toWaiAppPlain
    stats <- statsMiddleware (appEnv conf) trackerPool
    ui <- (enforceVhost . stats . autohead . etagMiddleware) <$>
          toWaiAppPlain foundation
    return $ measureDuration $ anyApp [tracker, ui]
  where
    anyApp [app] = 
        app
    anyApp (app:apps) = 
        \req -> app req >>= \res ->
        let tryNext =
                case res of
                  Wai.ResponseFile (Status 404 _) _ _ _ ->
                      True
                  Wai.ResponseBuilder (Status 404 _) _ _ ->
                      True
                  Wai.ResponseSource (Status 404 _) _ _ ->
                      True
                  _ ->
                      False
        in if tryNext
           then anyApp apps req
           else return res
    anyApp [] =
        return $ 
        return $ 
        Wai.ResponseBuilder (Status 404 "Not found") [] mempty
    enforceVhost :: Wai.Middleware
    enforceVhost app req =
        let proceed = app req
            getRedirectResponse location = return $
                                Wai.ResponseBuilder (Status 301 "Wrong vhost")
                                [("Location", location)]
                                mempty
        in case "Host" `lookup` Wai.requestHeaders req of
             Nothing ->
                 proceed
             Just vhost
                 | any (`BC.isPrefixOf` vhost) ["localhost", "bitlove.org", "api.bitlove.org"] ->
                     proceed
             Just _ ->
                 getRedirectResponse $ 
                 "http://bitlove.org" `BC.append` Wai.rawPathInfo req
    measureDuration :: Wai.Middleware
    measureDuration app req =
        do cpu1 <- liftIO getCPUTime
           utc1 <- liftIO getCurrentTime
           res <- app req
           let res' = res `seq` res
           _ <- register $ do
             cpu2 <- liftIO getCPUTime
             utc2 <- liftIO getCurrentTime
             liftIO $ BC.hPutStrLn stderr $ BC.concat
                      [ "["
                      , BC.pack $ show (truncate $ (utc2 `diffUTCTime` utc1) * 1000 :: Int)
                      , "ms real "
                      , BC.pack $ show ((cpu2 - cpu1) `div` 1000000000)
                      , "ms cpu] "
                      , BC.pack $ show $ statusCode $ Wai.responseStatus res
                      , " "
                      , Wai.requestMethod req
                      , " "
                      , Wai.rawPathInfo req
                      ]
           return $ res'
           
    etagMiddleware app req =
      do let reqETags = 
                 "If-None-Match" `lookup` Wai.requestHeaders req
             matches etag = 
                 case (etag `BC.breakSubstring`) <$> reqETags of
                   Just (_, reqEtags')
                       | not (BC.null reqEtags') ->
                           True
                   _ ->
                       False
         res <- app req
         let may304 res s hs
                 | statusCode s == 200 &&
                   maybe False matches ("ETag" `lookup` hs) =
                       return $ Wai.ResponseBuilder notModified304 hs mempty
                 | otherwise =
                       return res
         case res of
           Wai.ResponseFile s hs _ _ -> 
               may304 res s hs
           Wai.ResponseBuilder s hs _ -> 
               may304 res s hs
           Wai.ResponseSource s hs _ -> 
               may304 res s hs

getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Settings.Development)
        { csParseExtra = parseExtra 
        }

makeUIFoundation :: AppConfig BitloveEnv Extra -> DBPool -> IO UIApp
makeUIFoundation conf pool = do
    manager <- newManager def
    s <- staticSite
    return $ UIApp conf s pool manager
    
parseDBConf :: Monad m =>
               Value -> m [(String, String)]
parseDBConf = return . parse
  where parse (Aeson.Object o) = do
          (k, v) <- HashMap.toList o
          let k' = Text.unpack k
          case v of
            Aeson.String v' ->
              return (k', Text.unpack v')
            (Aeson.Object _) ->
              parse v
            (Aeson.Number n) ->
              return (k', show n)
            _ ->
              error ("Cannot parse: " ++ show v)
        parse _ = error "Expected JSON object"
    
makeDBPool :: [(String, String)] -> IO DBPool
makeDBPool dbconf =
  let dbconf' :: [([Char], [Char])]
      dbconf' = filter ((`elem` ["host", "hostaddr",
                                 "port", "dbname",
                                 "user", "password"]) . fst) dbconf
      dbconf'' = unwords $
                 map (\(k, v) ->
                       k ++ "=" ++ v
                     ) dbconf'
  in createPool
     (hPutStrLn stderr "connectPostgreSQL" >> connectPostgreSQL dbconf'')
     (\db -> hPutStrLn stderr "HDBC.disconnect" >> HDBC.disconnect db)
     4 60 4


getFaviconR :: GHandler sub UIApp ()
getFaviconR =
    redirectWith (Status 301 "Over there") $ StaticR $ StaticRoute ["favicon.png"] []
    