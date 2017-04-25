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
import Database.PostgreSQL.LibPQ (Connection)

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
  safeConvert [title, image, homepage] =
      UserDetails <$>
      safeConvert title <*>
      (fixUrl <$> safeConvert image) <*>
      safeConvert homepage
  safeConvert vals = convError "UserDetails" vals

userDetailsByName :: UserName -> Query UserDetails
userDetailsByName user =
    query "SELECT COALESCE(\"title\", ''), COALESCE(\"image\", ''), COALESCE(\"homepage\", '') FROM users WHERE \"name\"=$1"
    [convert user]

setUserDetails :: UserName -> UserDetails -> Connection -> IO ()
setUserDetails user details db = do
  Just _ <-
    query' "UPDATE users SET \"title\"=?, \"image\"=?, \"homepage\"=? WHERE \"name\"=?"
    [convert $ userTitle details, convert $ userImage details, convert $ userHomepage details, convert user]
    db
  return ()

newtype Salt = Salt { unSalt :: ByteString }

instance Convertible Salt SqlValue where
    safeConvert = safeConvert . unSalt

instance Convertible SqlValue Salt where
    safeConvert v = Salt <$> safeConvert v

instance ToJSON Salt where
    toJSON = toJSON . toHex . unSalt

newtype Salted = Salted { unSalted :: ByteString }

instance Convertible Salted SqlValue where
    safeConvert = safeConvert . unSalted

instance Convertible SqlValue Salted where
    safeConvert v = Salted <$> safeConvert v

instance ToJSON Salted where
    toJSON = toJSON . toHex . unSalted


data UserSalt = UserSalt Salt Salted
                deriving (Typeable)

instance Convertible [SqlValue] UserSalt where
  safeConvert [salt, salted] =
      UserSalt <$>
      safeConvert salt <*>
      safeConvert salted
  safeConvert vals = convError "UserSalt" vals

userSalt :: UserName -> Query UserSalt
userSalt user =
    query "SELECT \"salt\", \"salted\" FROM users WHERE \"name\"=?" [convert user]

setUserSalted :: UserName -> Salted -> Connection -> IO ()
setUserSalted user salted db = do
  Just _ <-
    query' "UPDATE users SET \"salted\"=? WHERE \"name\"=?"
    [convert salted, convert user] db
  return ()

registerUser :: UserName -> Text -> Connection -> IO (Maybe Salt)
registerUser username email db = do
  r <- query' "SELECT COUNT(\"name\") FROM users WHERE \"name\"=?"
       [convert username] db
  case r of
    Just [[v]] | convert v < (1 :: Int) -> do
           salt <- generateSalt
           Just _ <-
             query' "INSERT INTO users (\"name\", \"email\", \"salt\") VALUES (?, ?, ?)"
             [convert username, convert email, convert salt] db
           return $ Just salt
    _ ->
        return Nothing

    where generateSalt =
              (Salt . B.pack . take 8 . randoms) `fmap`
              getStdGen

userByEmail :: Text -> Query UserName
userByEmail email =
  query "SELECT \"name\" FROM users WHERE \"email\"=?"
  [convert email]
