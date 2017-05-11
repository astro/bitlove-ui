{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Feed where

import Prelude
import Data.Convertible
import Data.Text (Text)
import Data.Data (Typeable)
import Database.PostgreSQL.LibPQ (Connection)

import Utils
import Model.SqlValue
import Model.Query
import Model.User

userFeed :: UserName -> Text -> Query Text
userFeed user slug =
  query1 "SELECT \"feed\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
  [convert user, convert slug]

newtype FeedXml = FeedXml Text
    deriving (Typeable)

instance Convertible [SqlValue] FeedXml where
  safeConvert [v] = FeedXml <$> safeConvert v
  safeConvert vals = convError "FeedXml" vals

feedXml :: Text -> Query FeedXml
feedXml url =
  query "SELECT \"xml\" FROM feeds WHERE \"url\"=?" [convert url]

instance Convertible [SqlValue] (Text, Text) where
  safeConvert [val1, val2] =
      case (safeConvert val1, safeConvert val2) of
        (Right t1, Right t2) -> Right (t1, t2)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
  safeConvert vals = convError "Text tuple" vals

feedEnclosures :: Text -> Query (Text, Text)
feedEnclosures url =
  query "SELECT enclosures.url, torrents.name FROM enclosures JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE enclosures.feed=$1 ORDER BY enclosures.url ASC" [convert url]

enclosureErrors :: Text -> Query (Text, Text)
enclosureErrors url =
  query "SELECT enclosures.\"url\", enclosure_torrents.\"error\" FROM enclosures JOIN enclosure_torrents USING (url) WHERE enclosures.feed=? AND enclosure_torrents.\"error\" IS NOT NULL AND enclosure_torrents.\"error\" != ''" [convert url]

data FeedInfo = FeedInfo {
      feedUrl :: Text
    , feedUser :: UserName
    , feedSlug :: Text
    , feedTitle :: Text
    , feedHomepage :: Text
    , feedImage :: Text
    , feedPublic :: Bool
    , feedTorrentify :: Bool
    , feedError :: Maybe Text
    } deriving (Show, Typeable)

instance Convertible [SqlValue] FeedInfo where
  safeConvert [url, user, slug, title, homepage, image, public, torrentify, error_text] =
    FeedInfo <$>
    safeConvert url <*>
    safeConvert user <*>
    safeConvert slug <*>
    safeConvert title <*>
    safeConvert homepage <*>
    (fixUrl <$> safeConvert image) <*>
    safeConvert public <*>
    safeConvert torrentify <*>
    safeConvert error_text
  safeConvert vals = convError "FeedInfo" vals

userFeeds :: UserName -> Bool -> Query FeedInfo
userFeeds user isOwner =
  query ("SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? " ++
         (if isOwner
          then ""
          else "AND user_feeds.\"public\" ") ++
         "ORDER BY LOWER(feeds.\"title\") ASC"
         ) [convert user]

userFeedInfo :: UserName -> Text -> Query FeedInfo
userFeedInfo user slug =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=?" [convert user, convert slug]

addUserFeed :: UserName -> Text -> Text -> Connection -> IO Bool
addUserFeed user slug url db =
    convResult <$>
    query' "SELECT * FROM add_user_feed(?, ?, ?)"
    [convert user, convert slug, convert url] db
  where convResult (Just [[v]]) = convert v
        convResult _ = False

deleteUserFeed :: UserName -> Text -> Connection -> IO Bool
deleteUserFeed user slug db =
    (== [1 :: Int]) <$>
    query1 "DELETE FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
    [convert user, convert slug] db

feedByUrl :: Text -> Query FeedInfo
feedByUrl url =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM feeds INNER JOIN user_feeds ON feeds.url=user_feeds.feed WHERE feeds.url=? AND user_feeds.\"public\"=true" [convert url]

data FeedDetails = FeedDetails {
      fdPublic :: Bool,
      fdTitle :: Maybe Text
    } deriving (Show, Typeable)

instance Convertible [SqlValue] FeedDetails where
  safeConvert [public, title] =
    FeedDetails <$>
    safeConvert public <*>
    safeConvert title
  safeConvert vals = convError "FeedDetails" vals

userFeedDetails :: UserName -> Text -> Query FeedDetails
userFeedDetails user slug =
  query "SELECT COALESCE(\"public\", FALSE), \"title\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
            [convert user, convert slug]

setUserFeedDetails :: UserName -> Text -> FeedDetails -> Connection -> IO Bool
setUserFeedDetails user slug details db =
  (== [1::Int]) <$>
  query1 "UPDATE user_feeds SET \"public\"=?, \"title\"=? WHERE \"user\"=? AND \"slug\"=?"
  [convert $ fdPublic details, convert $ fdTitle details,
    convert user, convert slug]
  db

searchFeeds :: Text -> Query FeedInfo
searchFeeds needle =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\"  FROM search_feeds(?) AS feeds JOIN user_feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"public\"" [convert needle]
