{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Feed where

import Prelude
import Data.Convertible
import Database.HDBC
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Data (Typeable)

import Utils
import Model.Query
import Model.User

userFeed :: UserName -> Text -> Query Text
userFeed user slug =
  query "SELECT \"feed\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
  [toSql user, toSql slug]

newtype FeedXml = FeedXml LBC.ByteString
    deriving (Typeable)

instance Convertible [SqlValue] FeedXml where
  safeConvert [v] = FeedXml <$> safeConvert v
  safeConvert vals = convError "FeedXml" vals

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
  query "SELECT enclosures.url, torrents.name FROM enclosures JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE enclosures.feed=$1 ORDER BY enclosures.url ASC" [toSql url]
  
enclosureErrors :: Text -> Query (Text, Text)
enclosureErrors url =
  query "SELECT enclosures.\"url\", enclosure_torrents.\"error\" FROM enclosures JOIN enclosure_torrents USING (url) WHERE enclosures.feed=? AND enclosure_torrents.\"error\" IS NOT NULL AND enclosure_torrents.\"error\" != ''" [toSql url]
  
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
  safeConvert (url:user:slug:title:homepage:image:public:torrentify:error_text:[]) =
    FeedInfo <$>
    safeFromSql url <*>
    safeFromSql user <*>
    safeFromSql slug <*>
    safeFromSql title <*>
    safeFromSql homepage <*>
    (fixUrl <$> safeFromSql image) <*>
    safeFromSql public <*>
    safeFromSql torrentify <*>
    safeFromSql error_text
  safeConvert vals = convError "FeedInfo" vals

userFeeds :: UserName -> Bool -> Query FeedInfo
userFeeds user isOwner =
  query ("SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? " ++
         (if isOwner
          then ""
          else "AND user_feeds.\"public\" ") ++
         "ORDER BY LOWER(feeds.\"title\") ASC" 
         ) [toSql user]

userFeedInfo :: UserName -> Text -> Query FeedInfo
userFeedInfo user slug =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM user_feeds INNER JOIN feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=?" [toSql user, toSql slug]

addUserFeed :: IConnection conn => 
               UserName -> Text -> Text -> conn -> IO Bool
addUserFeed user slug url db =
    fmap (fromSql . head . head) $
    quickQuery' db "SELECT * FROM add_user_feed(?, ?, ?)"
                    [toSql user, toSql slug, toSql url]
       
deleteUserFeed :: IConnection conn => 
                  UserName -> Text -> conn -> IO Bool
deleteUserFeed user slug db =
    (== 1) `fmap`
    run db "DELETE FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
        [toSql user, toSql slug]
        
feedByUrl :: Text -> Query FeedInfo
feedByUrl url =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\" FROM feeds INNER JOIN user_feeds ON feeds.url=user_feeds.feed WHERE feeds.url=? AND user_feeds.\"public\"=true" [toSql url]
                    
data FeedDetails = FeedDetails {
      fdPublic :: Bool,
      fdTitle :: Maybe Text
    } deriving (Show, Typeable)
                    
instance Convertible [SqlValue] FeedDetails where
  safeConvert (public:title:[]) =
    FeedDetails <$>
    safeFromSql public <*>
    safeFromSql title
  safeConvert vals = convError "FeedDetails" vals

userFeedDetails :: UserName -> Text -> Query FeedDetails
userFeedDetails user slug =
  query "SELECT COALESCE(\"public\", FALSE), \"title\" FROM user_feeds WHERE \"user\"=? AND \"slug\"=?"
            [toSql user, toSql slug]
            
setUserFeedDetails :: IConnection conn => 
                      UserName -> Text -> FeedDetails -> conn -> IO Bool
setUserFeedDetails user slug details db =
  (== 1) `fmap`
  run db "UPDATE user_feeds SET \"public\"=?, \"title\"=? WHERE \"user\"=? AND \"slug\"=?"
          [toSql $ fdPublic details, toSql $ fdTitle details,
           toSql user, toSql slug]
                  
searchFeeds :: Text -> Query FeedInfo
searchFeeds needle =
  query "SELECT feeds.\"url\", user_feeds.\"user\", user_feeds.\"slug\", COALESCE(user_feeds.\"title\", feeds.\"title\", 'Untitled'), COALESCE(feeds.\"homepage\", ''), COALESCE(feeds.\"image\", ''), COALESCE(user_feeds.\"public\", FALSE), feeds.\"torrentify\", feeds.\"error\"  FROM search_feeds(?) AS feeds JOIN user_feeds ON user_feeds.feed=feeds.url WHERE user_feeds.\"public\"" [toSql needle]
