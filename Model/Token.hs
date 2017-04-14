module Model.Token where

import Prelude
import System.Random
import Data.ByteString hiding (take, concatMap, length)
import Data.Text (Text)
import Data.Convertible
import Data.Aeson
import qualified Database.PostgreSQL.LibPQ as PQ

import Model.SqlValue
import Model.Query
import Model.User
import Utils


newtype Token = Token { unToken :: ByteString }
    deriving (Show, Read, Eq, Ord)


instance Convertible Token SqlValue where
    safeConvert = Right . toBytea' . unToken
             
instance Convertible SqlValue Token where
    safeConvert = Right . Token . fromBytea'

instance ToJSON Token where
    toJSON = toJSON . toHex . unToken
    

generateToken :: Text -> UserName -> PQ.Connection -> IO Token
generateToken kind user db = do
  token <- makeRandomToken
  Just _ <- query' "INSERT INTO user_tokens (\"kind\", \"user\", \"token\", \"created\") VALUES (?, ?, ?, NOW())"
    [convert kind, convert user, convert token] db
  return token
  
makeRandomToken :: IO Token
makeRandomToken = (Token . pack . take 16 . randoms) `fmap`
                  newStdGen

validateToken :: Text -> Token -> Query UserName
validateToken kind token =
  query "DELETE FROM user_tokens WHERE \"kind\"=? AND \"token\"=? RETURNING \"user\""
  [convert kind, convert token]

peekToken :: Text -> Token -> Query UserName
peekToken kind token =
  query "SELECT \"user\" FROM user_tokens WHERE \"kind\"=? AND \"token\"=?"
  [convert kind, convert token]
  
