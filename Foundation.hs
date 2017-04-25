{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: "SET client_encoding = 'UTF8'"
module Foundation
    ( BitloveEnv (..)
    , UIApp (..)
    , Route (..)
    , UIAppMessage (..)
    , resourcesUIApp
    , Handler
    , Widget
    , getFullUrlRender
    , isMiro
    , generateETag
    , addFilterScript
    , withDB, withDBPool, DBPool, HasDB (..), Transaction
    , Period (..)
    --, maybeAuth
    --, requireAuth
    , module Settings
    , module Model
    , Database
    ) where

import Prelude
import System.IO (stderr, hPrint)
import Yesod
import Yesod.Static
import Control.Monad.Trans.Resource
--import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings (widgetFile, Extra (..), BitloveEnv (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Data.Pool
import Settings.StaticFiles
import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Network.Wai as Wai
import Data.ByteString.Char8 (isInfixOf)
import qualified Data.ByteString.Lazy as LB
import qualified Crypto.Hash.SHA1 as SHA1
import Database.PostgreSQL.LibPQ (Connection)

import Utils
import PathPieces
import BitloveAuth
import Model.Query


type Database = Connection
type DBPool = Pool Connection


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data UIApp = UIApp
    { settings :: AppConfig BitloveEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , uiDBPool :: DBPool -- ^ Database connection pool.
    , httpManager :: Manager
    }

-- Set up i18n messages. See the message folder.
mkMessage "UIApp" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "UIApp" $(parseRoutesFileNoCheck "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod UIApp where
    approot = ApprootRelative

    {-
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120
    -}

    makeSessionBackend app =
      do let withDB' :: Transaction a -> IO a
             withDB' = runResourceT . withDBPool (uiDBPool app)
         return $ Just $ sessionBackend withDB'

    defaultLayout widget = do
        mmsg <- getMessage
        msu <- sessionUser

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          addScript $ StaticR js_jquery_1_7_1_min_js
          addScript $ StaticR js_jquery_flot_js
          addScript $ StaticR js_graphs_js
          addScriptRemote "https://api.flattr.com/js/0.6/load.js?mode=auto&popout=0&button=compact"
          $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y s =
        Just $ uncurry (joinPath y "") $ renderRoute s


    errorHandler = errorHandler'

    -- The page to be redirected to when authentication is required.
    --authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody
    
    -- Grant any read request
    isAuthorized _ False = return Authorized
    -- Handlers.Auth: for everyone
    isAuthorized SignupR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized (ActivateR _) _ = return Authorized
    isAuthorized ReactivateR _ = return Authorized
    -- Handlers.Edit: for respective owners
    isAuthorized (UserDetailsR user) _ = authorizeFor user
    isAuthorized (UserFeedDetailsR user _) _ = authorizeFor user
    isAuthorized (UserFeedR user _) _ = authorizeFor user
    isAuthorized (TorrentFileR user _ _) _ = authorizeFor user
    -- Forbid by default
    isAuthorized _ True = return $ Unauthorized "Cannot modify this resource"

authorizeFor :: MonadHandler m => UserName -> m AuthResult
authorizeFor user = do
  canEdit' <- canEdit user
  return $ if canEdit'
           then Authorized
           else Unauthorized "Authorization denied"

-- We want full http://host URLs only in a few cases (feeds, API)
getFullUrlRender :: HandlerT UIApp IO (Route UIApp -> Text)
getFullUrlRender =
    do approot' <- appRoot <$> settings <$> getYesod
       (T.append approot' .) <$> getUrlRender

isMiro :: MonadHandler m => m Bool
isMiro = let
        userAgent = lookup "User-Agent" <$> Wai.requestHeaders <$> waiRequest
    in maybe False (isInfixOf "Miro/") <$> userAgent
    

errorHandler' :: ErrorResponse -> HandlerT UIApp IO TypedContent
errorHandler' NotFound =
  fmap toTypedContent $ defaultLayout $ do
    setTitle "Bitlove: Not found"
    let img = StaticR $ StaticRoute ["404.jpg"] []
    toWidget [hamlet|$newline always
              <article>
                <h2>Not Found
                <img src="@{img}">
                <p class="hint">Here's a kitten instead.
              |]
errorHandler' (PermissionDenied _) =
  fmap toTypedContent $ defaultLayout $ do
    setTitle "Bitlove: Permission denied"
    toWidget [hamlet|$newline always
              <h2>Permission denied
              |]
errorHandler' e = do
  liftIO $ hPrint stderr e
  fmap toTypedContent $ defaultLayout $
    do setTitle "Bitlove: Error"
       let img = StaticR $ StaticRoute ["500.jpg"] []
       toWidget [hamlet|$newline always
                 <article>
                   <h2>Oops
                   <img src="@{img}">
                 |]

generateETag :: MonadHandler m => LB.ByteString -> m ()
generateETag = addHeader "ETag" . 
               quote . 
               toHex . 
               SHA1.hashlazy
  where quote t = T.concat ["\"", t, "\""]

addFilterScript :: WidgetT UIApp IO ()
addFilterScript =
    addScript $ StaticR js_filter_js

-- | Database interface

class HasDB y where
    getDBPool :: HandlerT y IO DBPool
  
instance HasDB UIApp where
    getDBPool = uiDBPool <$> getYesod
  
type Transaction a = Connection -> IO a
    
-- How to run database actions.
withDB :: HasDB y => Transaction a -> HandlerT y IO a
withDB f = do
    pool <- getDBPool
    liftIO $ runResourceT $ withDBPool pool f

-- TODO: could use more of ResourceT
withDBPool :: DBPool -> Transaction a -> ResourceT IO a
withDBPool pool f = liftIO $ do
    (db, localPool) <- takeResource pool
    let f' = do
          txBegin db
          a <- f db
          txCommit db
          return $ Right a
    ea <- liftIO $
          E.catch f' $ \e -> do
            txRollback db
            return $ Left e
    case ea of
      Left (e :: E.SomeException) ->
          do destroyResource pool localPool db
             E.throw e
      Right a ->
          do putResource localPool db
             return a
