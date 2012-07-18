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
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Text.Shakespeare.Text (stext)
import Network.Mail.Mime
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON)
import Data.Char

import Import
import BitloveAuth
import qualified Model as Model
import Model.User


getSignupR :: Handler RepHtml
getSignupR =
    defaultLayout $ do
      setTitle "Bitlove: Signup"
      $(whamletFile "templates/signup.hamlet")
      
data SignupStatus = SignupOk | SignupConflict | SignupError
      
postSignupR :: Handler RepHtml
postSignupR = do
  -- Validation
  mUsername <- lookupPostParam "username"
  username <- case mUsername of
                Just username 
                    | validateUsername username ->
                        return $ UserName username
                _ ->
                    sendError "Invalid username"
  
  mEmail <- lookupPostParam "email"
  email <- case mEmail of
             Just email 
                 | T.any (== '@') email ->
                     return email
             _ ->
                 sendError "Invalid email address"
  mTos1 <- lookupPostParam "tos-1"
  mTos2 <- lookupPostParam "tos-2"
  case (mTos1, mTos2) of
    (Just "tos-1", Just "tos-2") ->
        return ()
    _ ->
        sendError "You need to agree to our terms. We do not wish to get sued."

  -- create user
  errorOrToken <- withDB $ \db ->
       do mSalt <- Model.registerUser username email db
          case mSalt of
            Nothing ->
              return $ Left "A user of that name already exists."
            Just _ ->
              Right `fmap` Model.generateToken "activate" username db

  sent <-
      case errorOrToken of
        Left e ->
            sendError e
        Right token ->
            do activateLink <- ("https://bitlove.org" `T.append`) `fmap`
                               ($ ActivateR token) `fmap`
                               getUrlRender
               sendMail username email "Welcome to Bitlove" $
                        TLE.encodeUtf8 [stext|
Welcome to Bitlove!

To complete signup visit the following link:
    #{activateLink}

Thanks for sharing
    The Bitlove Team
    |]

  case sent of
    True ->
        defaultLayout $ do
              setTitle "Bitlove: Signup"
              toWidget [hamlet|
                        <h2>Account activation pending
                        <p>Please check your mail to activate your account.
                        |]
    False ->
        -- TODO: unregister & rm token
        defaultLayout $ do
              setTitle "Error"
              toWidget [hamlet|
                        <h2>Sorry
                        <p>Sending mail failed. Please #
                          <a href="mailto:mail@bitlove.org">contact support!
                        |]
    where validateUsername :: Text -> Bool
          validateUsername name = T.length name >= 3 &&
                                  all (\c ->
                                           isAsciiLower c ||
                                           isDigit c ||
                                           c `elem` "-_"
                                      ) (T.unpack name)
          sendError :: Text -> Handler a
          sendError e = defaultLayout (do
                                        setTitle "Error"
                                        toWidget [hamlet|
                                                  <h2>Error
                                                  <p>#{e}
                                                  <p>
                                                    <a href="@{SignupR}">Retry
                                                  |]
                                      ) >>= 
                        sendResponse
  
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
      
getActivateR :: Token -> Handler RepHtml
getActivateR token = do
  mSalt <-
      withDB $ \db ->
      do users <- Model.peekToken "activate" token db
         case users of
           [] ->
             return Nothing
           user:_ ->
               do (Model.UserSalt salt _):_ <- Model.userSalt user db
                  return $ Just salt
               
  case mSalt of
    Nothing ->
        defaultLayout $ do
                       setTitle "Huh?"
                       toWidget [hamlet|
                                 <h2>Error
                                 <p>Invalid activation token
                                 |]
    Just salt ->
        do let hexToken = toHex $ unToken token
               hexSalt = toHex $ unSalt salt
           defaultLayout $ do
                       setTitle "Bitlove: Peer-to-Peer Love for Your Podcast Downloads"
                       $(whamletFile "templates/activate.hamlet")

postActivateR :: Token -> Handler RepJson
postActivateR token = do
  Just salted <- (fmap $ Salted . fromHex) `fmap`
                 lookupPostParam "salted"
  mUser <- withDB $ \db ->
    do users <- Model.validateToken "activate" token db
       case users of
         [] ->
             return Nothing
         user:_ ->
             do Model.setUserSalted user salted db
                return $ Just user
          
  case mUser of
    Nothing ->
      returnJsonError "Invalid activation token"
    Just user ->
        login user >>
        (((:[]) . ("welcome" .=) . ($ UserR user)) `fmap`
         getUrlRender) >>=
        returnJson

getReactivateR :: Handler RepHtml
getReactivateR =
    defaultLayout $ do
      setTitle "Bitlove: Recover your password"
      $(whamletFile "templates/reactivate.hamlet")
      
postReactivateR :: Handler RepHtml
postReactivateR = do
  email <- fromMaybe "" `fmap`
           lookupPostParam "email"
  userTokens <- withDB $ \db ->
    do users <- Model.userByEmail email db
       foldM (\userTokens user ->
               do token <- Model.generateToken "activate" user db
                  return $ (user, token) : userTokens
             ) [] users
            
  sent <- foldM (\sent (user, token) ->
                     case sent of
                       False ->
                           return False
                       True  ->
                           do activateLink <- ("https://bitlove.org" `T.append`) `fmap`
                                              ($ ActivateR token) `fmap`
                                              getUrlRender
                              sendMail user email "Reactivate your Bitlove account" $
                                       TLE.encodeUtf8 [stext|
Welcome back to Bitlove!

To reset your account password visit the following link:
    #{activateLink}

Thanks for sharing
    The Bitlove Team
    |]
                ) True userTokens
  
  defaultLayout $ do
    setTitle "Bitlove: Recover your password"
    case sent of
      _ | null userTokens ->
            toWidget [hamlet|
                      <h2>Error
                      <p>No user with that email address was found.
                      |]
      False ->
          toWidget [hamlet|
                    <h2>Sorry
                    <p>Sending mail failed. Please #
                      <a href="mailto:mail@bitlove.org">contact support!
                    |]
      True ->
          toWidget [hamlet|
                    <h2>Account activation pending
                    <p>Please check your mail to activate your account.
                    |]
      
    
getLogoutR :: Handler ()
getLogoutR =
  logout >>
  (($ LoginR) `fmap` getUrlRender) >>=
  redirect


-- returns whether this was successful
sendMail :: UserName -> Text -> Text -> LBC.ByteString -> Handler Bool
sendMail toUser toEmail subject body =
    liftIO $
    E.catch (send >> return True) $
    \(E.ErrorCall _) -> return False
  where send =
            renderSendMail
            Mail {
              mailFrom = Address (Just "Bitlove.org") "mail@bitlove.org",
              mailTo = [Address (Just $ userName toUser) toEmail],
              mailCc = [],
              mailBcc = [],
              mailHeaders = [("Subject", subject)],
              mailParts = [[Part {
                              partType = "text/plain",
                              partEncoding = None,
                              partFilename = Nothing,
                              partHeaders = [],
                              partContent = body
                            }]]
            }

returnJson :: forall (m :: * -> *) a.
              (Monad m, ToJSON a) =>
              [(Text, a)] -> m RepJson
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
    