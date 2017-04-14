module Model.SqlValue where

import Prelude
import Data.Typeable (Typeable)
import Data.Convertible
import Data.Time.LocalTime (LocalTime)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Database.PostgreSQL.LibPQ (Oid(Oid))
import qualified PostgreSQL.Binary.Decoder as PD
import qualified PostgreSQL.Binary.Encoder as PE

data SqlValue = SqlValue Oid B.ByteString
              | SqlNull
  deriving (Show, Eq)

safeConvertSql :: (Typeable a) => B.ByteString -> PD.Decoder a -> ConvertResult a
safeConvertSql bytes parser =
  case PD.run parser bytes of
    Right a ->
      Right a
    Left errorText ->
      let errorText' = T.unpack errorText
      in convError errorText' bytes

sqlConverter :: (Typeable a) => [(Oid, PD.Decoder a)] -> SqlValue -> ConvertResult a
sqlConverter parsers (SqlValue oid bytes) =
  case oid `lookup` parsers of
    Just parser ->
      safeConvertSql bytes parser
    Nothing ->
      convError "Cannot handle Oid" oid

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
                PE.run PE.bool

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
                PE.run PE.text_strict

instance Convertible SqlValue Int where
  safeConvert = sqlConverter
                [ (Oid 20, PD.int)
                ]

instance Convertible Int SqlValue where
  safeConvert = Right .
                SqlValue (Oid 20) .
                PE.run PE.int8_int64 .
                convert

instance Convertible SqlValue Integer where
  safeConvert = sqlConverter
                [ (Oid 20, PD.int)
                ]

instance Convertible SqlValue LocalTime where
  safeConvert = sqlConverter
                [ (Oid 1114, PD.timestamp_float)
                ]

instance Convertible LocalTime SqlValue where
  safeConvert = Right .
                SqlValue (Oid 1114) .
                PE.run PE.timestamp_float
