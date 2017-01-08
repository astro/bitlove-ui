{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TupleSections #-}
module Model.User where

import Prelude
import Data.Convertible
import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Aeson
import System.Random
import Control.Applicative
import qualified Database.PostgreSQL.LibPQ as PQ

import Model.SqlValue
import Model.Query
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
    safeConvert vals = convError "UserName" vals

instance Read UserName where
    readsPrec _ = (:[]) . (, "") . UserName . T.pack
  


data UserDetails = UserDetails {
      userTitle :: Text,
      userImage :: Text,
      userHomepage :: Text
    } deriving (Typeable)
                   
instance Convertible [SqlValue] UserDetails where
  safeConvert (title:image:homepage:[]) =
      UserDetails <$>
      safeFromSql title <*>
      (fixUrl <$> safeFromSql image) <*>
      safeFromSql homepage
  safeConvert vals = convError "UserDetails" vals

userDetailsByName :: UserName -> Query UserDetails
userDetailsByName user =
    query "SELECT COALESCE(\"title\", ''), COALESCE(\"image\", ''), COALESCE(\"homepage\", '') FROM users WHERE \"name\"=$1"
    [toSql user]

setUserDetails :: UserName -> UserDetails -> PQ.Connection -> IO ()
setUserDetails user details db = do
  1 <- run db "UPDATE users SET \"title\"=?, \"image\"=?, \"homepage\"=? WHERE \"name\"=?"
       [toSql $ userTitle details, toSql $ userImage details, toSql $ userHomepage details, toSql user]
  return ()
  
newtype Salt = Salt { unSalt :: ByteString }

instance Convertible Salt SqlValue where
    safeConvert = Right . toBytea' . unSalt
             
instance Convertible SqlValue Salt where
    safeConvert = Right . Salt . fromBytea'

instance ToJSON Salt where
    toJSON = toJSON . toHex . unSalt

newtype Salted = Salted { unSalted :: ByteString }

instance Convertible Salted SqlValue where
    safeConvert = Right . toBytea' . unSalted
             
instance Convertible SqlValue Salted where
    safeConvert = Right . Salted . fromBytea'

instance ToJSON Salted where
    toJSON = toJSON . toHex . unSalted


data UserSalt = UserSalt Salt Salted
                deriving (Typeable)

instance Convertible [SqlValue] UserSalt where
  safeConvert (salt:salted:[]) =
      UserSalt <$>
      safeFromSql salt <*>
      safeFromSql salted
  safeConvert vals = convError "UserSalt" vals

userSalt :: UserName -> Query UserSalt
userSalt user =
    query "SELECT \"salt\", \"salted\" FROM users WHERE \"name\"=?" [toSql user]
    
setUserSalted :: UserName -> Salted -> PQ.Connection -> IO ()
setUserSalted user salted db = do
  1 <- run db
       "UPDATE users SET \"salted\"=? WHERE \"name\"=?"
       [toSql salted, toSql user]
  return ()
                 
registerUser :: UserName -> Text -> PQ.Connection -> IO (Maybe Salt)
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
  
