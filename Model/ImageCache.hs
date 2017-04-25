{-# LANGUAGE FlexibleInstances #-}
module Model.ImageCache where

import Prelude
import Data.Convertible
import Data.Text (Text)
import Model.SqlValue
import qualified Data.ByteString.Lazy as LB

import Foundation
import Model.Query

data CachedImage = CachedImage LB.ByteString
                 | CachedError Text

instance Convertible [SqlValue] CachedImage where
    safeConvert [img, err] =
      let img' = safeConvert img
      in case img' of
           Right img'' | not (LB.null img'') ->
             Right $ CachedImage img''
           Right _ ->
             CachedError <$> safeConvert err
           Left _ ->
             CachedError <$> safeConvert err
    safeConvert _ = Right $ CachedError "safeConvert CachedImage error"

getImage :: Text -> Int -> Query CachedImage
getImage url size =
    query "SELECT \"data\", \"error\" FROM cached_images WHERE \"url\"=? AND \"size\"=?" 
    [convert url, convert size]

putImage :: Text -> Int -> CachedImage -> Database -> IO ()
putImage url size cached db =
    do Just _ <-
         case cached of
              CachedError err ->
                  query' "INSERT INTO cached_images (\"url\", \"size\", \"error\", \"time\") VALUES (?, ?, ?, NOW())" 
                  [convert url, convert size, convert err] db
              CachedImage img ->
                  query' "INSERT INTO cached_images (\"url\", \"size\", \"data\", \"time\") VALUES (?, ?, ?, NOW())"
                  [convert url, convert size, convert img] db
       return ()
