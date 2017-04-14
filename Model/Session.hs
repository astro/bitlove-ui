{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Session where

import Prelude
import qualified Data.ByteString as B
import Data.Convertible
import Data.Data (Typeable)
import System.Random
import qualified Database.PostgreSQL.LibPQ as PQ

import Model.SqlValue
import Model.Query
import Model.User

    
newtype SessionId = SessionId { unSessionId :: B.ByteString }
                  deriving (Show, Eq, Typeable)

instance Convertible SessionId SqlValue where
  safeConvert = Right . toBytea' . unSessionId
  
instance Convertible SqlValue SessionId where
  safeConvert = Right . SessionId . fromBytea'

instance Convertible [SqlValue] SessionId where
  safeConvert [v] = safeConvert v
  safeConvert vs = convError "SessionId" vs
  
createSession :: UserName -> PQ.Connection -> IO SessionId
createSession user db = do
  sid <- makeRandomSid
  Just _ <- query' "INSERT INTO user_sessions (\"user\", \"sid\", \"updated\") VALUES (?, ?, NOW())" [convert user, convert sid] db
  return sid
  
makeRandomSid :: IO SessionId
makeRandomSid = (SessionId . B.pack . take 32 . randoms) `fmap`
                newStdGen


validateSession :: SessionId -> Query UserName
validateSession sid = 
  query "UPDATE user_sessions SET \"updated\"=NOW() WHERE \"sid\"=? RETURNING \"user\"" [convert sid]

invalidateSession :: SessionId -> PQ.Connection -> IO ()
invalidateSession sid db = do 
  Just _ <- query' "DELETE FROM user_sessions WHERE \"sid\"=?" [convert sid] db
  return ()
