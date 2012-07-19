{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Stats where

import Prelude
import Data.Convertible
import Data.Data (Typeable)
import Database.HDBC
import Data.Text (Text)
import Data.Time (LocalTime)

import Model.Query
import Model.Download (InfoHash)

data StatsValue = StatsValue LocalTime Double
                deriving (Show, Typeable)

instance Convertible [SqlValue] StatsValue where
  safeConvert (time:val:[]) =
    Right $
    StatsValue
    (fromSql time)
    (fromSql val)
  safeConvert vals = convError "StatsValue" vals
  
getCounter :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getCounter kind info_hash start stop interval =
  query "SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / ?) * ?) AS t, SUM(\"value\") FROM counters WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC" [toSql interval, toSql interval, toSql kind, toSql info_hash, toSql start, toSql stop]
  
addCounter :: IConnection conn => 
              Text -> InfoHash -> Integer -> conn -> IO ()
addCounter kind infoHash increment db = do
  _ <- run db
       "SELECT * FROM add_counter(?, ?, ?)"
       [toSql kind, toSql infoHash, toSql increment]
  return ()

getGauge :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
getGauge kind info_hash start stop interval =
  query ("SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / ?) * ?) AS t, " ++ agg ++ "(\"value\") FROM gauges WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC") [toSql interval, toSql interval, toSql kind, toSql info_hash, toSql start, toSql stop]
  where agg = 
            case kind of
                "leechers" -> "MIN"
                _ -> "MAX"
                
