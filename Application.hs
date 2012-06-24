{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import Network.HTTP.Conduit (newManager, def)
import Data.Conduit.Pool
import Database.HDBC as HDBC (disconnect)
import Database.HDBC.PostgreSQL
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Debug.Trace


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Browse

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment 
              "config/postgresql.yml"
              (appEnv conf)
              parseDBConf
    pool <- makeDBPool dbconf
    return $ App conf setLogger s pool manager
    
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
              trace ("Cannot parse: " ++ show v) []
    
makeDBPool :: [(String, String)] -> IO DBPool
makeDBPool dbconf =
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
     (connectPostgreSQL dbconf'')
     HDBC.disconnect
     4 5 4

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
