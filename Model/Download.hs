{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Download where

import Prelude
import Data.Convertible
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Data.ByteString (ByteString, unpack)
import Data.Data (Typeable)
import Numeric (showHex)

import Utils
import Model.SqlValue
import Model.Query
import Model.User

newtype InfoHash = InfoHash { unInfoHash :: ByteString }
                 deriving (Read, Eq, Ord, Hashable)

instance Show InfoHash where
  show = T.unpack . toHex . unInfoHash

infoHashToHex :: InfoHash -> Text
infoHashToHex (InfoHash bs) =
  T.pack $
  concatMap (flip showHex "") $
  unpack bs

instance Convertible InfoHash SqlValue where
  safeConvert (InfoHash bs) = safeConvert bs

instance Convertible SqlValue InfoHash where
  safeConvert = (InfoHash <$>) . safeConvert

-- For selecting just the info_hash column:
instance Convertible [SqlValue] InfoHash where
  safeConvert = safeConvert . head

data Download = Download {
  downloadUser :: UserName,
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
  downloadDownloaded :: Integer
} deriving (Show, Typeable)

instance Convertible [SqlValue] Download where
  safeConvert [user, slug, feed, item, enclosure,
               feed_title, _feed_public,
               info_hash, name, size, type_,
               title, lang, summary, published,
               homepage, payment, image,
               downloaded] =
    Download <$>
    safeConvert user <*>
    safeConvert slug <*>
    safeConvert feed <*>
    safeConvert item <*>
    safeConvert enclosure <*>
    safeConvert info_hash <*>
    safeConvert name <*>
    safeConvert size <*>
    safeConvert type_ <*>
    safeConvert feed_title <*>
    safeConvert title <*>
    safeConvert lang <*>
    safeConvert summary <*>
    safeConvert published <*>
    safeConvert homepage <*>
    safeConvert payment <*>
    (fixUrl <$> safeConvert image) <*>
    safeConvert downloaded
  safeConvert vals = convError "Download" vals

downloadFields :: String
downloadFields =
  "\"user\", \"slug\", \"feed\", \"item\", \"enclosure\", \"info_hash\", \"name\", \"size\", \"type\", \"feed_title\", \"title\", \"lang\", \"summary\", \"published\", \"homepage\", \"payment\", \"image\", \"downloaded\""

recentDownloads :: QueryPage -> Query Download
recentDownloads page =
  query ("SELECT " ++ downloadFields ++ " FROM get_recent_downloads(?, ?)") $
  convert page

mostDownloaded :: Int -> QueryPage -> Query Download
mostDownloaded period page =
  query ("SELECT " ++ downloadFields ++ " FROM get_most_downloaded(?, ?, ?)") $
  convert page ++ [convert period]

userDownloads :: UserName -> QueryPage -> Query Download
userDownloads user page =
  query ("SELECT " ++ downloadFields ++ " FROM get_user_recent_downloads(?, ?, ?)") $
  convert page ++ [convert user]

enclosureDownloads :: Text -> Query Download
enclosureDownloads url =
  query ("SELECT " ++ downloadFields ++ " FROM get_enclosure_downloads(?)") [convert url]

guidDownloads :: Text -> Query Download
guidDownloads guid =
  query ("SELECT " ++ downloadFields ++ " FROM get_guid_downloads(?)") [convert guid]

newtype GUID = GUID { unGUID :: Text }

instance Convertible [SqlValue] GUID where
  safeConvert [guid] = GUID <$> safeConvert guid
  safeConvert vals = convError "GUID" vals

torrentGuids :: InfoHash -> Query Text
torrentGuids infoHash =
  (map unGUID <$>) .
  query "SELECT * FROM get_torrent_guids(?)" [convert infoHash]

torrentDownloads :: InfoHash -> Query Download
torrentDownloads =
  query ("SELECT " ++ downloadFields ++ " FROM get_torrent_download(?)") .
  (:[]) . convert

feedDownloads :: Text -> QueryPage -> Query Download
feedDownloads url page =
  query ("SELECT " ++ downloadFields ++ " FROM get_recent_downloads(?, ?, ?)") $
  convert page ++ [convert url]

searchDownloads :: Text -> QueryPage -> Query Download
searchDownloads needle page =
  query ("SELECT " ++ downloadFields ++ " FROM search_feed_items(?, ?, ?)") $
  convert page ++ [convert needle]

newtype EnclosureURL = EnclosureURL { unEnclosureURL :: Text }

instance Convertible [SqlValue] EnclosureURL where
  safeConvert [guid] = EnclosureURL <$> safeConvert guid
  safeConvert vals = convError "EnclosureURL" vals

torrentEnclosures :: InfoHash -> Query Text
torrentEnclosures infoHash =
  (map unEnclosureURL <$>) .
  query "SELECT url FROM enclosure_torrents WHERE info_hash=?"
  [convert infoHash]
