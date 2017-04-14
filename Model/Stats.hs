{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Stats where

import Prelude
import Data.Convertible
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Time (LocalTime)
import Control.Applicative
import Database.PostgreSQL.LibPQ (Connection)

import Model.SqlValue
import Model.Query
import Model.Download (InfoHash)

data StatsValue = StatsValue LocalTime Double
                deriving (Show, Typeable)

instance Convertible [SqlValue] StatsValue where
  safeConvert (time:val:[]) =
    StatsValue <$>
    safeConvert time <*>
    safeConvert val
  safeConvert vals = convError "StatsValue" vals
  
getCounter :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getCounter kind info_hash start stop interval =
  query "SELECT align_timestamp(\"time\", ?) AS t, SUM(\"value\") FROM counters WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC" [convert interval, convert kind, convert info_hash, convert start, convert stop]
  
addCounter :: Text -> InfoHash -> Integer -> Connection -> IO ()
addCounter kind infoHash increment db = do
  _ <- query'
       "SELECT * FROM add_counter(?, ?, ?)"
       [convert kind, convert infoHash, convert increment] db
  return ()

getDownloadCounter :: Text -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getDownloadCounter path start stop interval =
  -- | "info_hash LIKE 'GET /%.torrent'" to use VIEW counters_get
  query "SELECT align_timestamp(\"time\", ?) AS t, SUM(\"value\") FROM counters WHERE info_hash LIKE 'GET /%.torrent' AND info_hash='GET '||?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC" [convert interval, convert path, convert start, convert stop]

getGauge :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getGauge kind info_hash start stop interval =
  query ("SELECT align_timestamp(\"time\", ?) AS t, MAX(\"value\") FROM gauges WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC") [convert interval, convert kind, convert info_hash, convert start, convert stop]
