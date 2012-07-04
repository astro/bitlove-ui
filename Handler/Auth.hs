{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Auth where

import Prelude
import Yesod
import Data.Conduit
import Crypto.Conduit (sinkHmac)
import Crypto.HMAC (MacKey (..))
import Crypto.Hash.SHA1
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Serialize (encode)
import Control.Monad
import Control.Monad.Error
import qualified Data.Text as T

import Import
import BitloveAuth
import qualified Model as Model
import Model.User


instance Error T.Text where
    noMsg = T.pack "Error"
    strMsg = T.pack

getSignupR :: Handler RepHtml
getSignupR =
    defaultLayout $ do
      setTitle "Bitlove: Signup"
      $(whamletFile "templates/signup.hamlet")
      
postSignupR :: Handler RepHtml
postSignupR = do
  mUsername <- lookupPostParam "username"
  mEmail <- lookupPostParam "email"
  mTos1 <- lookupPostParam "tos-1"
  mTos2 <- lookupPostParam "tos-2"
  validation :: Either Text (UserName, Text) <- 
                runErrorT $ do
                  username <- 
                      case mUsername of
                        Just username | T.length username >= 3 ->
                            return $ UserName username
                        _ ->
                            throwError "Invalid username"
                  email <-
                      case mEmail of
                        Just email | T.any (== '@') email ->
                            return email
                        _ ->
                            throwError "Invalid email address"
                  case (mTos1, mTos2) of
                    (Just "tos-1", Just "tos-2") ->
                        return ()
                    _ ->
                        throwError "You need to agree to our terms. We do not wish to get sued."
                  return (username, email)
  case validation of
    Left e ->
      defaultLayout $ do
        setTitle "Error"
        toWidget [hamlet|
                  <h2>Error
                  <p>#{e}
                  <p>
                    <a href="@{SignupR}">Retry
                  |]
    Right (username, email) ->
      -- TODO: create user, send mail
      defaultLayout $ do
        setTitle "Bitlove: Signup"
        toWidget [hamlet|
                  TODO
                  |]

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
             salts <- userSalt user db
             case salts of
               [] ->
                 return Nothing
               (UserSalt salt _):_ ->
                 do token <- Model.generateToken "login" user db
                    return $ Just (salt, token)
           case mst of
             Nothing ->
                 returnJsonError "No such user"
             Just (salt, token) ->
                 returnJson ["salt" .= salt,
                             "token" .= token
                            ]
    (Nothing, Just hexToken, Just hexResponse) ->
        do r <-
             withDB $ \db -> do
               let token = Token $ fromHex hexToken
               users <- Model.validateToken "login" token db
               case users of
                 [] ->
                     return $ Left "Invalid token"
                 user:_ -> 
                     do (UserSalt _ salted):_ <- userSalt user db
                        let hexSalted = toHex $ unSalted salted
                        expected <- runResourceT $ hmacSHA1 (unToken token) (encodeUtf8 hexSalted)
                        case fromHex hexResponse of
                          response
                              | response == expected ->
                                -- Success!
                                return $ Right user
                          _ ->
                            return $ Left "Wrong password"
           case r of
             Left msg ->
                 returnJsonError msg
             Right user ->
               do login user
                  welcomeLink <- ($ UserR user) `fmap`
                                 getUrlRender
                  returnJson ["welcome" .= welcomeLink]
    _ ->
      returnJsonError "Protocol error"
      
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

returnJsonError :: Text -> Handler RepJson
returnJsonError = returnJson . (:[]) . ("error" .=)


hmacSHA1 :: ByteString -> ByteString -> ResourceT IO ByteString
hmacSHA1 keyData msgData = do
    let key = MacKey keyData
    d <-
           yield msgData $$
                     (sinkHmac key :: Sink ByteString (ResourceT IO) SHA1)
    return $ encode d
    