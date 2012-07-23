{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , makeUIFoundation
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getFaviconR)
import Yesod.Logger (Logger, logBS, toProduction, logString)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import Network.HTTP.Conduit (newManager, def)
import Data.Conduit.Pool
import Database.HDBC as HDBC (disconnect)
import Database.HDBC.PostgreSQL
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types (Status (Status, statusCode))
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import System.IO (stderr)
import Network.Wai.Middleware.Autohead


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
mkYesodDispatch "UIApp" resourcesUIApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig BitloveEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    dbconf <- withYamlEnvironment 
              "config/postgresql.yml"
              (appEnv conf)
              parseDBConf
    uiPool <- makeDBPool dbconf setLogger
    trackerPool <- makeDBPool dbconf setLogger
    foundation <- makeUIFoundation conf uiPool setLogger
    tracker <- makeTrackerApp trackerPool >>= toWaiAppPlain
    stats <- statsMiddleware (appEnv conf) trackerPool
    ui <- (enforceVhost . stats . autohead) `fmap` 
          toWaiAppPlain foundation
    return $ measureDuration $ {-logWare $-} anyApp [tracker, ui]
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)
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
            redirect location = return $
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
                 redirect $ 
                 "http://bitlove.org" `BC.append` Wai.rawPathInfo req
    measureDuration :: Wai.Middleware
    measureDuration app req =
        do cpu1 <- liftIO getCPUTime
           utc1 <- liftIO getCurrentTime
           res <- app req
           let res' = res `seq` res
           cpu2 <- liftIO getCPUTime
           utc2 <- liftIO getCurrentTime
           liftIO $ BC.hPutStrLn stderr $ BC.concat
             [ "["
             , BC.pack $ show ((cpu2 - cpu1) `div` 1000000000)
             , "ms cpu "
             , BC.pack $ show (truncate $ (utc2 `diffUTCTime` utc1) * 1000 :: Int)
             , "ms real] "
             , BC.pack $ show $ statusCode $ Wai.responseStatus res
             , " "
             , Wai.requestMethod req
             , " "
             , Wai.rawPathInfo req
             ]
           return $ res'

makeUIFoundation :: AppConfig BitloveEnv Extra -> DBPool -> Logger -> IO UIApp
makeUIFoundation conf pool setLogger = do
    manager <- newManager def
    s <- staticSite
    return $ UIApp conf setLogger s pool manager
    
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
    
makeDBPool :: [(String, String)] -> Logger -> IO DBPool
makeDBPool dbconf logger =
  let dbconf' :: [([Char], [Char])]
      dbconf' = filter ((`elem` ["host", "hostaddr",
                                 "port", "dbname",
                                 "user", "password"]) . fst) dbconf
      dbconf'' = unwords $
                 map (\(k, v) ->
                       k ++ "=" ++ v
                     ) dbconf'
  in createPool
     (logString logger "connectPostgreSQL" >> connectPostgreSQL dbconf'')
     (\db -> logString logger "HDBC.disconnect" >> HDBC.disconnect db)
     4 60 4


getFaviconR :: GHandler sub UIApp ()
getFaviconR =
    redirectWith (Status 301 "Over there") $ StaticR $ StaticRoute ["favicon.png"] []
    