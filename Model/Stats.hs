{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Stats where

import Prelude
import Data.Convertible
import Data.Data (Typeable)
import Database.HDBC
import Data.Text (Text)
import Data.Time (LocalTime)
import Control.Applicative

import Model.Query
import Model.Download (InfoHash)

data StatsValue = StatsValue LocalTime Double
                deriving (Show, Typeable)

instance Convertible [SqlValue] StatsValue where
  safeConvert (time:val:[]) =
    StatsValue <$>
    safeFromSql time <*>
    safeFromSql val
  safeConvert vals = convError "StatsValue" vals
  
getCounter :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getCounter kind info_hash start stop interval =
  query "SELECT align_timestamp(\"time\", ?) AS t, SUM(\"value\") FROM counters WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND align_timestamp(\"time\", ?)>=? AND align_timestamp(\"time\", ?)<=? GROUP BY t ORDER BY t ASC" [toSql interval, toSql kind, toSql info_hash, toSql interval, toSql start, toSql interval, toSql stop]
  
addCounter :: IConnection conn => 
              Text -> InfoHash -> Integer -> conn -> IO ()
addCounter kind infoHash increment db = do
  _ <- run db
       "SELECT * FROM add_counter(?, ?, ?)"
       [toSql kind, toSql infoHash, toSql increment]
  return ()

getDownloadCounter :: Text -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getDownloadCounter path start stop interval =
  -- | "info_hash LIKE 'GET /%.torrent'" to use VIEW counters_get
  query "SELECT align_timestamp(\"time\", ?) AS t, SUM(\"value\") FROM counters WHERE info_hash LIKE 'GET /%.torrent' AND info_hash='GET '||?::BYTEA AND align_timestamp(\"time\", ?)>=? AND align_timestamp(\"time\", ?)<=? GROUP BY t ORDER BY t ASC" [toSql interval, toSql path, toSql interval, toSql start, toSql interval, toSql stop]

getGauge :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getGauge kind info_hash start stop interval =
  query ("SELECT align_timestamp(\"time\", ?) AS t, MAX(\"value\") FROM gauges WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND align_timestamp(\"time\", ?)>=? AND align_timestamp(\"time\", ?)<=? GROUP BY t ORDER BY t ASC") [toSql interval, toSql kind, toSql info_hash, toSql interval, toSql start, toSql interval, toSql stop]
