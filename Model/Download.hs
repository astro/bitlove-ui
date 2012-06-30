{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Download where

import Data.Convertible
import Prelude
import Database.HDBC


import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Data.ByteString (ByteString, unpack)
import Data.Data (Typeable)
import Numeric (showHex)


import Model.Query

newtype InfoHash = InfoHash ByteString
                 deriving (Show)
                          
infoHashToHex :: InfoHash -> Text
infoHashToHex (InfoHash bs) =
  T.pack $
  concatMap (\byte ->
              showHex byte ""
            ) $
  unpack bs
                          
instance Convertible InfoHash SqlValue where
  safeConvert (InfoHash bs) = Right $ toBytea bs

instance Convertible SqlValue InfoHash where
  safeConvert = Right . InfoHash . fromBytea

-- For selecting just the info_hash column:
instance Convertible [SqlValue] InfoHash where
  safeConvert = safeConvert . head

data Download = Download {
  downloadUser :: Text,
  downloadSlug :: Text,
  downloadFeed :: Text,
  downloadItem :: Text,
  downloadEnclosure :: Maybe Text,
  downloadInfoHash :: InfoHash,
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
} deriving (Show, Typeable)

instance Convertible [SqlValue] Download where
  safeConvert (user:slug:feed:item:enclosure:
               feed_title:feed_public:
               info_hash:name:size:type_:
               title:lang:summary:published:
               homepage:payment:image:
               seeders:leechers:upspeed:downspeed:downloaded:[]) = 
    Right $
    Download (fromSql user) (fromSql slug) (fromSql feed) (fromSql item) (fromSql enclosure)
    (fromSql info_hash) (fromSql name) (fromSql size) (fromSql type_) 
    (fromSql feed_title) (fromSql title) (fromSql lang) (fromSql summary) (fromSql published) 
    (fromSql homepage) (fromSql payment) (fromSql image) 
    (fromSql seeders) (fromSql leechers) (fromSql upspeed) (fromSql downspeed) (fromSql downloaded)
  safeConvert vals = convError "Download" vals


recentDownloads :: Int -> Query Download
recentDownloads limit =
  query "SELECT * FROM get_recent_downloads(?)" [toSql limit]

popularDownloads :: Int -> Query Download
popularDownloads limit =
  query "SELECT * FROM get_popular_downloads(?)" [toSql limit]

mostDownloaded :: Int -> Int -> Query Download
mostDownloaded limit period =
  query "SELECT * FROM get_most_downloaded(?, ?)" [toSql limit, toSql period]

userDownloads :: Int -> Text -> Query Download
userDownloads limit user =
  query "SELECT * FROM get_user_recent_downloads(?, ?)" [toSql limit, toSql user]

enclosureDownloads :: Text -> Query Download
enclosureDownloads url =
  query "SELECT * FROM get_enclosure_downloads(?)" [toSql url]
  
feedDownloads :: Int -> Text -> Query Download
feedDownloads limit url =
  query "SELECT * FROM get_recent_downloads(?, ?)" [toSql limit, toSql url]
  