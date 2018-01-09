-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( widgetFile
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , BitloveEnv (..)
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml
import Settings.Development
import Data.Default (def)
import Text.Hamlet
import qualified Data.ByteString.Char8 as BC


data BitloveEnv = Development
                | Production4 
                | Production6 
                | Production4SSL
                | Production6SSL 
                deriving (Read, Show, Enum, Bounded)

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   https://developers.google.com/speed/docs/best-practices/request#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright :: Text
    , extraCert :: Maybe String
    , extraKey :: Maybe String
    , extraContactMail :: Text
    , extraServedVhosts :: [BC.ByteString]
    , extraTrackerURLs :: [BC.ByteString]
    , extraExportAuth :: [Text]
    } deriving Show

parseExtra :: BitloveEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "cert"
    <*> o .:? "key"
    <*> o .: "contact_mail"
    <*> (map encodeUtf8 <$> o .: "served_vhosts")
    <*> (map encodeUtf8 <$> o .: "tracker_urls")
    <*> o .: "export_auth"
