{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.HDBC
import Data.Convertible
import Data.Time.LocalTime (LocalTime)
import Data.ByteString (ByteString)


class Structurable e where
  structureResult :: [SqlValue] -> e


data Download = Download {
  downloadUser :: Text,
  downloadSlug :: Text,
  downloadFeed :: Text,
  downloadItem :: Text,
  downloadEnclosure :: Maybe Text,
  downloadInfoHash :: ByteString,
  downloadName :: Text,
  downloadSize :: Integer,
  downloadType :: Text,
  downloadFeedTitle :: Maybe Text,
  downloadTitle :: Text,
  downloadLang :: Maybe Text,
  downloadSummary :: Maybe Text,
  downloadPublished :: LocalTime,
  downloadHomepage :: Text,
  downloadPayment :: Text,
  downloadImage :: Text,
  downloadSeeders :: Integer,
  downloadLeechers :: Integer,
  downloadUpspeed :: Integer,
  downloadDownspeed :: Integer,
  downloadDownloaded :: Integer
} deriving (Show{-, Typeable-})

instance Structurable Download where
  structureResult (user:slug:feed:item:enclosure:
                   feed_title:feed_public:
                   info_hash:name:size:type_:
                   title:lang:summary:published:
                   homepage:payment:image:
                   seeders:leechers:upspeed:downspeed:downloaded:_) = 
    Download (fromSql user) (fromSql slug) (fromSql feed) (fromSql item) (fromSql enclosure)
    (fromSql info_hash) (fromSql name) (fromSql size) (fromSql type_) 
    (fromSql feed_title) (fromSql title) (fromSql lang) (fromSql summary) (fromSql published) 
    (fromSql homepage) (fromSql payment) (fromSql image) 
    (fromSql seeders) (fromSql leechers) (fromSql upspeed) (fromSql downspeed) (fromSql downloaded)
  structureResult vals = error $ "Cannot structure " ++ show vals


query :: (IConnection conn,
          Convertible arg SqlValue,
          Structurable e
         ) => String -> [arg] -> conn -> IO [e]
query sql args conn =
  (map structureResult) `fmap`
  (quickQuery' conn sql (map toSql args))
  
  
{-groupDownloads :: [Download] -> [[Download]]
groupDownloads-}

type DownloadsQuery = IConnection conn => conn -> IO [Download]

recentDownloads :: Int -> DownloadsQuery
recentDownloads limit =
  query "SELECT * FROM get_recent_downloads(?)" [limit]

popularDownloads :: Int -> DownloadsQuery
popularDownloads limit =
  query "SELECT * FROM get_popular_downloads(?)" [limit]

mostDownloaded :: Int -> Int -> DownloadsQuery
mostDownloaded limit period =
  query "SELECT * FROM get_most_downloaded(?, ?)" [limit, period]
