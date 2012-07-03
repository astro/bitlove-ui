{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TupleSections #-}
module Model.User where

import Prelude (Either (Right), ($), Eq, Ord, Show, fmap, Read (..), (.))
import Data.Convertible
import Data.Data (Typeable)
import Data.Text (Text, pack)
import Database.HDBC
import Model.Query


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
    readsPrec _ = (:[]) . (, "") . UserName . pack
  


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

data UserSalt = UserSalt Text Text

instance Convertible [SqlValue] UserSalt where
  safeConvert (salt:salted:[]) =
      Right $
      UserSalt
      (fromSql salt)
      (fromSql salted)

userSalt :: UserName -> Query UserSalt
userSalt user =
    query "SELECT \"salt\", \"salted\" FROM users WHERE \"name\"=?" [toSql user]
    