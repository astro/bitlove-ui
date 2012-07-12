{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeUIFoundation
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
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
import Network.HTTP.Types (Status (Status))

import BitloveAuth

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Auth
import Handler.Browse
import Handler.Edit
import Handler.TorrentFile
import Handler.TorrentStats
import Handler.MapFeed
import Handler.DownloadFeeds
import Handler.ByEnclosureAPI
import Handler.Tracker

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "UIApp" resourcesUIApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeUIFoundation conf setLogger
    tracker <- toWaiAppPlain $ makeTrackerApp $ uiDBPool foundation
    ui <- toWaiAppPlain foundation
    return $ logWare $ anyApp [tracker, ui]
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

makeUIFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO UIApp
makeUIFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment 
              "config/postgresql.yml"
              (appEnv conf)
              parseDBConf
    pool <- makeDBPool dbconf setLogger
    return $ UIApp conf setLogger s pool manager
    
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
    
makeDBPool :: [(String, String)] -> Logger -> IO DBPool
makeDBPool dbconf logger =
  let dbconf' :: [([Char], [Char])]
      dbconf' = filter (\(k, v) ->
                         k `elem` ["host", "hostaddr",
                                   "port", "dbname",
                                   "user", "password"]
                       ) dbconf
      dbconf'' = unwords $
                 map (\(k, v) ->
                       k ++ "=" ++ v
                     ) dbconf'
  in createPool
     (logString logger "connectPostgreSQL" >> connectPostgreSQL dbconf'')
     (\db -> logString logger "HDBC.disconnect" >> HDBC.disconnect db)
     4 5 4

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
