{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Feed where

import Prelude
import Data.Convertible
import Database.HDBC
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Data (Typeable)

import Model.Query
import Model.User

instance Convertible [SqlValue] Text where
  safeConvert = safeConvert . head

user_feed :: UserName -> Text -> Query Text
user_feed user slug =
  query "SELECT \"feed\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
  [toSql user, toSql slug]

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
  
data FeedDetails = FeedDetails {
      feedUrl :: Text
    , feedSlug :: Text
    , feedTitle :: Text
    , feedHomepage :: Text
    , feedImage :: Text
    , feedPublic :: Bool
    , feedTorrentify :: Bool
    , feedError :: Maybe Text
    } deriving (Show, Typeable)
               
instance Convertible [SqlValue] FeedDetails where
  safeConvert (url:slug:title:homepage:image:public:torrentify:error:[]) =
    Right $
    FeedDetails 
    (fromSql url)
    (fromSql slug)
    (fromSql title) 
    (fromSql homepage) 
    (fromSql image) 
    (fromSql public) 
    (fromSql torrentify) 
    (fromSql error)
  safeConvert vals = convError "FeedDetails" vals

userFeeds :: UserName -> Bool -> Query FeedDetails
userFeeds user isOwner =
  query ("SELECT feeds.\"url\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\"), feeds.\"homepage\", feeds.\"image\", COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? " ++
         (if isOwner
          then ""
          else "AND user_feeds.\"public\" ") ++
         "ORDER BY LOWER(feeds.\"title\") ASC" 
         ) [toSql user]

userFeedDetails :: UserName -> Text -> Query FeedDetails
userFeedDetails user slug =
  query "SELECT feeds.\"url\", ?::TEXT, COALESCE(user_feeds.\"title\", feeds.\"title\"), feeds.\"homepage\", feeds.\"image\", COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=?" [toSql slug, toSql user, toSql slug]
