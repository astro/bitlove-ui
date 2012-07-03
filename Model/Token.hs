module Model.Token where

import Prelude
import System.Random
import Data.ByteString hiding (take, concatMap, length)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Convertible
import Database.HDBC
import Numeric (showHex)
import Data.Aeson

import Model.Query
import Model.User
import Utils


newtype Token = Token { unToken :: ByteString }
    deriving (Show, Eq, Ord)


instance Convertible Token SqlValue where
    safeConvert = safeConvert . toHex . unToken
             
instance Convertible SqlValue Token where
    safeConvert v = (Token . fromHex) `fmap` safeConvert v

instance ToJSON Token where
    toJSON = toJSON . toHex . unToken

generateToken :: IConnection conn => Text -> UserName -> conn -> IO Token
generateToken kind user db = do
  token <- makeRandomToken
  1 <- run db "INSERT INTO user_tokens (\"kind\", \"user\", \"token\", \"created\") VALUES (?, ?, ?, NOW())" [toSql kind, toSql user, toSql token]
  return token
  
makeRandomToken :: IO Token
makeRandomToken = (Token . pack . take 32 . randoms) `fmap`
                  getStdGen

validateToken :: Text -> Token -> Query UserName
validateToken kind token =
  query "DELETE FROM user_tokens WHERE \"kind\"=$1 AND \"token\"=$2 RETURNING \"user\""
  [toSql kind, toSql token]
