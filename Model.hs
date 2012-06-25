{-# LANGUAGE RankNTypes, FlexibleInstances, DeriveDataTypeable, ConstraintKinds, ImpredicativeTypes #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC
import Data.Convertible
import Data.Time.LocalTime (LocalTime)
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC
import Data.Data (Typeable)
import Numeric (readHex, showOct)
import Data.Char (chr)


type Query e = IConnection conn => conn -> IO [e]

query :: (IConnection conn,
          Convertible [SqlValue] e
         ) => String -> [SqlValue] -> conn -> IO [e]
query sql args conn = do
  rows <- quickQuery' conn sql args
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
                           
toBytea :: ByteString -> SqlValue
toBytea = toSql . BC.pack . concatMap (escape . fromIntegral) . unpack
  where escape 92 = "\\\\"
        escape c | c >= 32 && c <= 126 = [chr c]
        escape c = "\\" ++ oct c
        oct c = pad 3 '0' $ showOct c ""
        pad :: Int -> Char -> String -> String
        pad targetLen padding s
          | length s >= targetLen = s
          | otherwise = pad targetLen padding (padding:s)
                        
newtype InfoHash = InfoHash ByteString
                 deriving (Show)
                          
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


  
{-groupDownloads :: [Download] -> [[Download]]
groupDownloads-}


recentDownloads :: Int -> Query Download
recentDownloads limit =
  query "SELECT * FROM get_recent_downloads(?)" [toSql limit]

popularDownloads :: Int -> Query Download
popularDownloads limit =
  query "SELECT * FROM get_popular_downloads(?)" [toSql limit]

mostDownloaded :: Int -> Int -> Query Download
mostDownloaded limit period =
  query "SELECT * FROM get_most_downloaded(?, ?)" [toSql limit, toSql period]


infoHashByName :: Text -> Text -> Text -> Query InfoHash
infoHashByName user slug name =
  query "SELECT \"info_hash\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [toSql user, toSql slug, toSql name]

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

newtype FeedXml = FeedXml Text

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
  