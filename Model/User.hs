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
userDetailsByName name =
    query "SELECT COALESCE(\"title\", ''), COALESCE(\"image\", ''), COALESCE(\"homepage\", '') FROM users WHERE \"name\"=$1"
    [toSql name]
