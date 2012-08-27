{-# LANGUAGE TupleSections, RankNTypes #-}
module BitloveAuth where

import Prelude
import Yesod
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Network.Wai as Wai
import Web.Cookie
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)
import qualified Data.Text as T

import Utils
import Model.Session
import Model.User

login :: UserName -> GHandler y y' ()
login = setSession "user" . userName

sessionUser :: GHandler y y' (Maybe UserName)
sessionUser = lookupSession "user" >>=
              return . maybe Nothing (Just . UserName)
      
canEdit :: UserName -> GHandler sub master Bool
canEdit user = maybe False (`elem` [user, UserName "astro"]) `fmap`
               sessionUser
  
logout :: GHandler y y' ()
logout = deleteSession "user"


sessionBackend :: (forall b. (PostgreSQL.Connection -> IO b) -> IO b) -> SessionBackend a
sessionBackend withDB =
    -- | App callback
    SessionBackend $ \_app req _time ->
    do let mSidCookie =
               listToMaybe
               [sid
                | ("Cookie", headerValue) <- Wai.requestHeaders req,
                  sid <- maybeToList $ "sid" `lookup` parseCookies headerValue
               ]
           mSid 
             | maybe False isHex mSidCookie = 
                 (SessionId . fromHex . T.pack . BC.unpack) `fmap`
                 mSidCookie
             | otherwise = 
                 Nothing
           getBackendSession :: IO BackendSession
           getBackendSession =
               case mSid of
                 Nothing ->
                     return []
                 Just sid ->
                     do users <- withDB $ validateSession sid
                        return $
                               case users of
                                 (UserName user):_ ->
                                     [("user", BC.pack $ T.unpack user)]
                                 _ ->
                                     []

       oldSession <- getBackendSession
       let saveSession :: BackendSession -> time -> IO [Header]
           saveSession newSession _time =
               let mOldUser = "user" `lookup` oldSession
                   mNewUser = "user" `lookup` newSession
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
