import Prelude
import Yesod.Default.Config (AppConfig (..), loadConfig, configSettings, csParseExtra)
import Settings
import Application          (makeApplication)
import System.Environment (getArgs)
import Foundation (BitloveEnv)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
import Network.Wai.Handler.WarpTLS (runTLS, TLSSettings (..))

fromArgs :: IO (AppConfig BitloveEnv Settings.Extra)
fromArgs = do
  args <- getArgs
  case args of
    [e] -> 
        case reads e of
          (e', _):_ ->
              loadConfig $ 
              (configSettings e') { csParseExtra = parseExtra }
          _ ->
              error $ "Invalid environment: " ++ show e
    _ ->
        error "Missing environment argument"

main :: IO ()
main = do 
  config <- fromArgs 
  app <- makeApplication config
  print config
  let settings = defaultSettings
                        { settingsPort = appPort config
                        , settingsHost = appHost config
                        }
      extra = appExtra config
      run = case (extraCert extra, extraKey extra) of
              (Nothing, Nothing) ->
                  runSettings
              (Just cert, Just key) ->
                  runTLS $ TLSSettings cert key
              _ ->
                  error "Specify both cert and key for SSL"
  run settings app
  