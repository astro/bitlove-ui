module Handler.Auth where

import Prelude
import Yesod

import Import
import BitloveAuth
import qualified Model as Model

getLoginR :: Handler RepHtml
getLoginR =
    defaultLayout $ do
      setTitle "Bitlove: Peer-to-Peer Love for Your Podcast Downloads"
      $(whamletFile "templates/login.hamlet")


postLoginR :: Handler RepJson
postLoginR = do
  mUsername <- lookupPostParam "username"
  mHexToken <- lookupPostParam "token"
  mHexResponse <- lookupPostParam "response"
  
  case (mUsername, mHexToken, mHexResponse) of
    (Just username, Nothing, Nothing) ->
        do let user = UserName username
           mst <- withDB $ \db -> do
             salts <- Model.userSalt (Model.UserName username) db
             case salts of
               [] ->
                 return Nothing
               (UserSalt salt _):_ ->
                 do token <- Model.generateToken "login" user db
                    return $ Just (salt, token)
           case mst of
             Nothing ->
                 returnJson ["error" .= ("No such user" :: Text)]
             Just (salt, token) ->
                 -- TODO: hex!
                 returnJson ["salt" .= salt,
                             "token" .= token
                            ]
    (Nothing, Just hexToken, Just hexResponse) ->
      undefined
    _ ->
      returnJson ["error" .= ("Protocol error" :: Text)]
      
getActivateR :: Handler ()
getActivateR = undefined

postActivateR :: Handler ()
postActivateR = undefined

getLogoutR :: Handler ()
getLogoutR =
  logout >>
  (($ LoginR) `fmap` getUrlRender) >>=
  redirect


returnJson = return . RepJson . toContent . object
