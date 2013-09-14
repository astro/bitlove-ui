{-# LANGUAGE TupleSections, RankNTypes #-}
module BitloveAuth where

import Prelude
import Control.Applicative
import Yesod
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Network.Wai as Wai
import Web.Cookie
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)
import qualified Data.Text as T
import qualified Data.Map as Map

import Utils
import Model.Session
import Model.User

login :: UserName -> HandlerT y IO ()
login = setSession "user" . userName

sessionUser :: MonadHandler m => m (Maybe UserName)
sessionUser = maybe Nothing (Just . UserName) <$>
              lookupSession "user"
      
canEdit :: MonadHandler m => UserName -> m Bool
canEdit user = maybe False (`elem` [user, UserName "astro"]) <$>
               sessionUser
  
logout :: MonadHandler m => m ()
logout = deleteSession "user"


sessionBackend :: (forall b. (PostgreSQL.Connection -> IO b) -> IO b) -> SessionBackend
sessionBackend withDB =
    -- | App callback
    SessionBackend $ \req ->
    do let mSidCookie =
               listToMaybe
               [sid
                | ("Cookie", headerValue) <- Wai.requestHeaders req,
                  sid <- maybeToList $ "sid" `lookup` parseCookies headerValue
               ]
           mSid 
             | maybe False isHex mSidCookie = 
                 (SessionId . fromHex' . BC.unpack) `fmap`
                 mSidCookie
             | otherwise = 
                 Nothing
           getSessionData :: IO SessionMap
           getSessionData =
                  case mSid of
                    Nothing ->
                        return Map.empty
                    Just sid ->
                        do users <- withDB $ validateSession sid
                           return $ Map.fromList $
                               case users of
                                 (UserName user):_ ->
                                     [("user", BC.pack $ T.unpack user)]
                                 _ ->
                                     []

       oldSession <- getSessionData
       let saveSession :: SessionMap -> IO [Header]
           saveSession newSession =
               let mOldUser = "user" `Map.lookup` oldSession
                   mNewUser = "user" `Map.lookup` newSession
               in case (mOldUser, mNewUser) of
                    -- Login
                    (Nothing, Just user) ->
                        do session <- withDB $
                                      createSession $ 
                                      UserName $ T.pack $ BC.unpack user
                           putStrLn $ "New session for " ++ 
                                    show user ++ ": " ++
                                    show session
                           return [AddCookie def
                                   { setCookieName = "sid"
                                   , setCookieValue = BC.pack $ T.unpack $
                                                      toHex $
                                                            unSessionId
                                                            session
                                   }]
                    -- Logout
                    (Just _user, Nothing) ->
                        do case mSid of
                             Just sid ->
                                 withDB $ invalidateSession sid
                             _ ->
                                 return ()
                           return [DeleteCookie "sid" "/"]
                    _ ->
                        return []
       return (oldSession, saveSession)
