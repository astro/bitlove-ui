{-# LANGUAGE FlexibleInstances #-}
module Model.ImageCache where

import Prelude
import Data.Convertible
import Data.Text (Text)
import Database.HDBC
import qualified Data.ByteString as B

import Model.Query

data CachedImage = CachedImage B.ByteString
                 | CachedError String

instance Convertible [SqlValue] CachedImage where
    safeConvert [data_, err] 
        | not (B.null $ fromBytea data_) =
            Right $ CachedImage $ fromBytea data_
        | otherwise =
            Right $ CachedError $ fromSql err
    safeConvert _ = Right $ CachedError "safeConvert CachedImage error"

getImage :: Text -> Int -> Query CachedImage
getImage url size =
    query "SELECT \"data\", \"error\" FROM cached_images WHERE \"url\"=? AND \"size\"=?" 
    [toSql url, toSql size]

putImage :: IConnection conn => 
            Text -> Int -> CachedImage -> conn -> IO ()
putImage url size cached db =
    do 1 <- case cached of
              CachedError err ->
                  run db "INSERT INTO cached_images (\"url\", \"size\", \"error\", \"time\") VALUES (?, ?, ?, NOW())" 
                  [toSql url, toSql size, toSql err]
              CachedImage data_ ->
                  run db "INSERT INTO cached_images (\"url\", \"size\", \"data\", \"time\") VALUES (?, ?, ?, NOW())" 
                  [toSql url, toSql size, toBytea data_]
       return ()
