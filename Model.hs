{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model (
  -- Model
  infoHashByName,
  Torrent (..),
  torrentByName,
  purgeTorrent,
  DirectoryEntry (..),
  getDirectory,
  -- TODO: move, camelCase
  StatsValue (..),
  get_counter,
  get_gauge,
  -- Model.Download
  InfoHash (..),
  infoHashToHex,
  Download (..),
  recentDownloads,
  popularDownloads,
  mostDownloaded,
  userDownloads,
  enclosureDownloads,
  feedDownloads,
  -- Model.Item
  Item (..),
  groupDownloads,
  -- Model.User
  UserName (..),
  UserDetails (..),
  userDetailsByName,
  setUserDetails,
  UserSalt (..),
  userSalt,
  setUserSalted,
  registerUser,
  userByEmail,
  -- Model.Feed
  FeedXml (..),
  feedXml,
  feedEnclosures,
  FeedInfo (..),
  userFeed,
  userFeeds,
  userFeedInfo,
  addUserFeed,
  deleteUserFeed,
  FeedDetails (..),
  userFeedDetails,
  setUserFeedDetails,
  -- Model.Token
  Token (..),
  generateToken,
  validateToken,
  peekToken
  ) where

import Prelude
import Data.Convertible
import Data.Text (Text)
import Database.HDBC
import Data.Time
import Data.Data (Typeable)
import qualified Data.ByteString.Char8 as BC

import Model.Query
import Model.Download
import Model.Item
import Model.Feed
import Model.User
import Model.Token
                        

infoHashByName :: UserName -> Text -> Text -> Query InfoHash
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

torrentByName :: UserName -> Text -> Text -> Query Torrent
torrentByName user slug name =
  query "SELECT \"info_hash\", \"name\", \"size\", \"torrent\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [toSql user, toSql slug, toSql name]
  
purgeTorrent :: IConnection conn => 
                UserName -> Text -> Text -> conn -> IO Int
purgeTorrent user slug name db =
  (fromSql . head . head) `fmap`
  quickQuery' db "SELECT * FROM purge_download(?, ?, ?)"
              [toSql user, toSql slug, toSql name]

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
  
get_gauge :: Text -> InfoHash -> LocalTime -> LocalTime -> Integer -> Query StatsValue
get_gauge kind info_hash start stop interval =
  query ("SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM \"time\") / ?) * ?) AS t, " ++ agg ++ "(\"value\") FROM gauges WHERE \"kind\"=? AND \"info_hash\"=?::BYTEA AND \"time\">=? AND \"time\"<=? GROUP BY t ORDER BY t ASC") [toSql interval, toSql interval, toSql kind, toSql info_hash, toSql start, toSql stop]
  where agg = 
            case kind of
                "leechers" -> "AVG"
                _ -> "MAX"
                

data DirectoryEntry = DirectoryEntry
    { dirUser :: UserName
    , dirUserTitle :: Text
    , dirUserImage :: Text
    , dirFeedSlug :: Text
    , dirFeedTitle :: Text
    , dirFeedLang :: Text
    , dirFeedTypes :: Text
    } deriving (Show, Typeable)

instance Convertible [SqlValue] DirectoryEntry where
    safeConvert (user:userTitle:userImage:
                 feedSlug:feedTitle:feedLang:feedTypes:[]) = 
      Right $
      DirectoryEntry
      (fromSql user)
      (fromSql userTitle)
      (fromSql userImage)
      (fromSql feedSlug)
      (fromSql feedTitle)
      (fromSql feedLang)
      (fromSql feedTypes)
    safeConvert vals = convError "DirectoryEntry" vals
      
getDirectory :: Query DirectoryEntry
getDirectory =
  query "SELECT \"user\", COALESCE(\"title\", \"user\"), COALESCE(\"image\", ''), \"slug\", COALESCE(\"feed_title\", \"slug\"), COALESCE(\"lang\", ''), array_to_string(\"types\", ',', '') FROM directory" []

