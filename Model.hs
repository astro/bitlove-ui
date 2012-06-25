{-# LANGUAGE RankNTypes, FlexibleInstances, DeriveDataTypeable #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC
import Data.Convertible
import Data.Time.LocalTime (LocalTime)
import Data.ByteString (ByteString, pack)
import Data.Data (Typeable)
import Numeric (readHex)


type Query e = IConnection conn => conn -> IO [e]

query :: (IConnection conn,
          Convertible arg SqlValue,
          Convertible [SqlValue] e
         ) => String -> [arg] -> conn -> IO [e]
query sql args conn = do
  rows <- quickQuery' conn sql (map toSql args)
  return $ do Right val <- safeConvert `fmap` rows
              return val

fromBytea :: SqlValue -> ByteString
fromBytea = unescape . fromSql
  where unescape :: Text -> ByteString
        unescape text =
          case T.splitAt 2 text of
            ("\\x", hex) ->
              pack $ hexToWords hex
            _ ->
              error "Cannot unescape hexadecimal BYTEA"
        hexToWords text
          | T.null text = []
          | otherwise = let (hex, text') = T.splitAt 2 text
                            w = fst $ head $ readHex $ T.unpack hex
                        in w:(hexToWords text')
              
newtype InfoHash = InfoHash ByteString
                 deriving (Show)
                          
instance Convertible SqlValue InfoHash where
  safeConvert = Right . InfoHash . fromBytea

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


  
{-groupDownloads :: [Download] -> [[Download]]
groupDownloads-}


recentDownloads :: Int -> Query Download
recentDownloads limit =
  query "SELECT * FROM get_recent_downloads(?)" [limit]

popularDownloads :: Int -> Query Download
popularDownloads limit =
  query "SELECT * FROM get_popular_downloads(?)" [limit]

mostDownloaded :: Int -> Int -> Query Download
mostDownloaded limit period =
  query "SELECT * FROM get_most_downloaded(?, ?)" [limit, period]


data Torrent = Torrent {
  torrentInfoHash :: InfoHash,
  torrentName :: Text,
  torrentSize :: Integer,
  torrentTorrent :: ByteString
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
  query "SELECT \"info_hash\", \"name\", \"size\", \"torrent\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [user, slug, name]
  