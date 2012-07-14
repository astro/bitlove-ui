{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Session where

import Prelude
import Database.HDBC
import qualified Data.ByteString as B
import Data.Convertible
import Data.Data (Typeable)
import System.Random

import Model.Query
import Model.User

    
newtype SessionId = SessionId { unSessionId :: B.ByteString }
                  deriving (Show, Eq, Typeable)

instance Convertible SessionId SqlValue where
  safeConvert = Right . toBytea . unSessionId
  
instance Convertible SqlValue SessionId where
  safeConvert = Right . SessionId . fromBytea

instance Convertible [SqlValue] SessionId where
  safeConvert [v] = safeConvert v
  safeConvert vs = convError "SessionId" vs
  
createSession :: IConnection conn => UserName -> conn -> IO SessionId
createSession user db = do
  sid <- makeRandomSid
  1 <- run db "INSERT INTO user_sessions (\"user\", \"sid\", \"updated\") VALUES (?, ?, NOW())" [toSql user, toSql sid]
  return sid
  
makeRandomSid :: IO SessionId
makeRandomSid = (SessionId . B.pack . take 32 . randoms) `fmap`
                newStdGen


validateSession :: SessionId -> Query UserName
validateSession sid = 
  query "UPDATE user_sessions SET \"updated\"=NOW() WHERE \"sid\"=? RETURNING \"user\"" [toSql sid]

invalidateSession :: IConnection conn => SessionId -> conn -> IO ()
invalidateSession sid db = do 
  _ <- run db "DELETE FROM user_sessions WHERE \"sid\"=?" [toSql sid]
  return ()