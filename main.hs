import Prelude
import Yesod.Default.Config (AppConfig, loadConfig, configSettings, csParseExtra)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra, Extra)
import Application          (makeApplication)
import System.Environment (getArgs)
import Foundation (BitloveEnv)

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
main = defaultMain fromArgs makeApplication
