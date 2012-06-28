{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.User where

import Prelude (Either (Right), ($))
import Data.Convertible
import Data.Text (Text)
import Database.HDBC
import Model.Query


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

userDetailsByName :: Text -> Query UserDetails
userDetailsByName name =
    query "SELECT COALESCE(\"title\", ''), COALESCE(\"image\", ''), COALESCE(\"homepage\", '') FROM users WHERE \"name\"=$1"
    [toSql name]
