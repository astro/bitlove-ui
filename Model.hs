{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model (
  -- Model
  infoHashByName,
  Torrent (..),
  torrentByName,
  StatsValue (..),
  -- TODO: move, camelCase
  get_counter,
  user_feed,
  FeedXml (..),
  feedXml,
  feedEnclosures,
  -- Model.Download
  Download (..),
  recentDownloads,
  popularDownloads,
  mostDownloaded,
  userDownloads,
  enclosureDownloads,
  -- Model.Item
  Item (..),
  groupDownloads,
  -- Model.User
  UserDetails (..),
  userDetailsByName
  ) where

import Prelude
import Data.Convertible
import Data.Text (Text)
import Database.HDBC
import Data.Time
import Data.Data (Typeable)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC

import Model.Query
import Model.Download
import Model.Item
import Model.User
                        

infoHashByName :: Text -> Text -> Text -> Query InfoHash
infoHashByName user slug name =
  query "SELECT \"info_hash\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [toSql user, toSql slug, toSql name]

data Torrent = Torrent {
  torrentInfoHash :: InfoHash,
  torrentName :: Text,
  torrentSize :: Integer,
  torrentTorrent :: BC.ByteString
} deriving (Show, Typeable)

instance Convertible [SqlValue] Torrent where
  safeConvert (info_hash:name:size:torrent:[]) =
    Right $
    Torrent 
    (fromSql info_hash)
    (fromSql name)
    (fromSql size)
    (fromBytea torrent)
  safeConvert vals = convError "Torrent" vals

torrentByName :: Text -> Text -> Text -> Query Torrent
torrentByName user slug name =
  query "SELECT \"info_hash\", \"name\", \"size\", \"torrent\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [toSql user, toSql slug, toSql name]
  

data StatsValue = StatsValue LocalTime Double
                deriving (Show, Typeable)

instance Convertible [SqlValue] StatsValue where
  safeConvert (time:val:[]) =
    Right $
    StatsValue
    (fromSql time)
    (fromSql val)
  safeConvert vals = convError "StatsValue" vals
  
get_counter :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
get_counter kind info_hash start stop interval =
  query "SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / ?) * ?) AS t, SUM(\"value\") FROM counters WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC" [toSql interval, toSql interval, toSql kind, toSql info_hash, toSql start, toSql stop]

instance Convertible [SqlValue] Text where
  safeConvert = safeConvert . head

user_feed :: Text -> Text -> Query Text
user_feed feed slug =
  query "SELECT \"feed\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
  [toSql feed, toSql slug]

newtype FeedXml = FeedXml LBC.ByteString

instance Convertible [SqlValue] FeedXml where
  safeConvert = Right . FeedXml . fromSql . head

feedXml :: Text -> Query FeedXml
feedXml url =
  query "SELECT \"xml\" FROM feeds WHERE \"url\"=?" [toSql url]
  
instance Convertible [SqlValue] (Text, Text) where
  safeConvert (val1:val2:[]) = 
      case (safeConvert val1, safeConvert val2) of
        (Right t1, Right t2) -> Right (t1, t2)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
  safeConvert vals = convError "Text tuple" vals

feedEnclosures :: Text -> Query (Text, Text)
feedEnclosures url =
  query "SELECT enclosures.url, torrents.name FROM enclosures JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE enclosures.feed=$1" [toSql url]
  