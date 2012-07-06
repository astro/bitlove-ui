{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TupleSections #-}
module Model.User where

import Prelude
import Data.Convertible
import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC
import Model.Query
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Aeson
import System.Random

import Utils


newtype UserName = UserName { userName :: Text }
    deriving (Eq, Ord, Show, Typeable)
             
instance Convertible UserName SqlValue where
    safeConvert (UserName name) = safeConvert name

instance Convertible SqlValue UserName where
    safeConvert v = UserName `fmap` safeConvert v

-- | Single record row
instance Convertible [SqlValue] UserName where
    safeConvert [v] = UserName `fmap` safeConvert v

instance Read UserName where
    readsPrec _ = (:[]) . (, "") . UserName . T.pack
  


data UserDetails = UserDetails {
      userTitle :: Text,
      userImage :: Text,
      userHomepage :: Text
    }
                   
instance Convertible [SqlValue] UserDetails where
  safeConvert (title:image:homepage:[]) =
      Right $
      UserDetails
      (fromSql title)
      (fromSql image)
      (fromSql homepage)

userDetailsByName :: UserName -> Query UserDetails
userDetailsByName user =
    query "SELECT COALESCE(\"title\", ''), COALESCE(\"image\", ''), COALESCE(\"homepage\", '') FROM users WHERE \"name\"=$1"
    [toSql user]


newtype Salt = Salt { unSalt :: ByteString }

instance Convertible Salt SqlValue where
    safeConvert = Right . toBytea . unSalt
             
instance Convertible SqlValue Salt where
    safeConvert = Right . Salt . fromBytea

instance ToJSON Salt where
    toJSON = toJSON . toHex . unSalt

newtype Salted = Salted { unSalted :: ByteString }

instance Convertible Salted SqlValue where
    safeConvert = Right . toBytea . unSalted
             
instance Convertible SqlValue Salted where
    safeConvert = Right . Salted . fromBytea

instance ToJSON Salted where
    toJSON = toJSON . toHex . unSalted


data UserSalt = UserSalt Salt Salted

instance Convertible [SqlValue] UserSalt where
  safeConvert (salt:salted:[]) =
      Right $
      UserSalt
      (fromSql salt)
      (fromSql salted)

userSalt :: UserName -> Query UserSalt
userSalt user =
    query "SELECT \"salt\", \"salted\" FROM users WHERE \"name\"=?" [toSql user]
    
setUserSalted :: IConnection conn => 
                 UserName -> Salted -> conn -> IO ()
setUserSalted user salted db = do
  1 <- run db
       "UPDATE users SET \"salted\"=? WHERE \"name\"=?"
       [toSql salted, toSql user]
  return ()
                 
registerUser :: IConnection conn => 
                UserName -> Text -> conn -> IO (Maybe Salt)
registerUser username email db = do
  r <- quickQuery' db 
       "SELECT COUNT(\"name\") FROM users WHERE \"name\"=?" 
       [toSql username]
  case r of
    [[v]] | (fromSql v :: Int) < 1 -> do
           salt <- generateSalt
           1 <- run db 
                "INSERT INTO users (\"name\", \"email\", \"salt\") VALUES (?, ?, ?)"
                [toSql username, toSql email, toSql salt]
           return $ Just salt
    _ ->
        return Nothing
    
    where generateSalt =
              (Salt . B.pack . take 8 . randoms) `fmap`
              getStdGen
              
userByEmail :: Text -> Query UserName
userByEmail email =
  query "SELECT \"name\" FROM users WHERE \"email\"=?"
  [toSql email]
  