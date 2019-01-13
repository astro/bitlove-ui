{-# LANGUAGE FlexibleInstances #-}
module Model.SqlValue where

import Prelude
import Data.Typeable (Typeable)
import Data.Convertible
import Data.Time.LocalTime
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Database.PostgreSQL.LibPQ (Oid(Oid))
import qualified PostgreSQL.Binary.Decoding as PD
import qualified PostgreSQL.Binary.Encoding as PE
import qualified BinaryParser
import BinaryParser (BinaryParser)
import System.IO.Unsafe (unsafePerformIO)

data SqlValue = SqlValue Oid B.ByteString
              | SqlNull
  deriving (Show, Eq)

safeConvertSql :: (Typeable a) => B.ByteString -> BinaryParser a -> ConvertResult a
safeConvertSql bytes parser =
  case BinaryParser.run parser bytes of
    Right a ->
      Right a
    Left errorText ->
      let errorText' = T.unpack errorText
      in convError errorText' bytes

sqlConverter :: (Typeable a) => [(Oid, BinaryParser a)] -> SqlValue -> ConvertResult a
sqlConverter parsers (SqlValue oid bytes) =
  case oid `lookup` parsers of
    Just parser ->
      safeConvertSql bytes parser
    Nothing ->
      convError "Cannot handle Oid" oid
sqlConverter _ SqlNull =
      convError "Cannot handle NULL" ("SqlNull" :: String)

instance (Convertible SqlValue c) => Convertible SqlValue (Maybe c) where
  safeConvert SqlNull = Right Nothing
  safeConvert v = Right $ Just $ convert v

instance (Convertible c SqlValue) => Convertible (Maybe c) SqlValue where
  safeConvert Nothing = Right SqlNull
  safeConvert (Just v) = safeConvert v

instance Convertible SqlValue Bool where
  safeConvert = sqlConverter
                [ (Oid 16, PD.bool)
                ]

instance Convertible Bool SqlValue where
  safeConvert = Right .
                SqlValue (Oid 16) .
                PE.encodingBytes .
                PE.bool

instance Convertible SqlValue T.Text where
  safeConvert = sqlConverter
                [ (Oid 25, PD.text_strict)
                , (Oid 19, PD.text_strict)
                , (Oid 1043, PD.text_strict)
                , (Oid 1042, PD.text_strict)
                ]

instance Convertible T.Text SqlValue where
  safeConvert = Right .
                SqlValue (Oid 25) .
                PE.encodingBytes .
                PE.text_strict

instance Convertible SqlValue B.ByteString where
  safeConvert = sqlConverter [(Oid 17, PD.bytea_strict)]

instance Convertible B.ByteString SqlValue where
  safeConvert = Right .
                SqlValue (Oid 17) .
                PE.encodingBytes .
                PE.bytea_strict

instance Convertible SqlValue LB.ByteString where
  safeConvert = sqlConverter [(Oid 17, PD.bytea_lazy)]

instance Convertible LB.ByteString SqlValue where
  safeConvert = Right .
                SqlValue (Oid 17) .
                PE.encodingBytes .
                PE.bytea_lazy

instance Convertible SqlValue Int where
  safeConvert = sqlConverter
                [ (Oid 23, PD.int)
                , (Oid 21, PD.int)
                , (Oid 20, PD.int)
                ]

instance Convertible Int SqlValue where
  safeConvert = Right .
                SqlValue (Oid 23) .
                PE.encodingBytes .
                PE.int4_int32 .
                convert

instance Convertible SqlValue Integer where
  safeConvert = sqlConverter
                [ (Oid 23, PD.int)
                , (Oid 21, PD.int)
                , (Oid 20, PD.int)
                ]

instance Convertible Integer SqlValue where
  safeConvert = Right .
                SqlValue (Oid 20) .
                PE.encodingBytes .
                PE.int8_int64 .
                convert

instance Convertible SqlValue Float where
  safeConvert = sqlConverter
                [ (Oid 700, PD.float4)
                , (Oid 701, convert <$> PD.float8)
                ]

instance Convertible SqlValue Double where
  safeConvert = sqlConverter
                [ (Oid 700, convert <$> PD.float4)
                , (Oid 701, PD.float8)
                ]

instance Convertible SqlValue LocalTime where
  safeConvert = sqlConverter
                [ (Oid 1114, PD.timestamp_int)
                , (Oid 1184, utcToLocal <$> PD.timestamptz_int)
                ]
    where utcToLocal =
            let tz = unsafePerformIO getCurrentTimeZone
            in utcToLocalTime tz

instance Convertible LocalTime SqlValue where
  safeConvert = Right .
                SqlValue (Oid 1114) .
                PE.encodingBytes .
                PE.timestamp_int
