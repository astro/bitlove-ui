{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model (
  -- Model
  infoHashByName,
  infoHashExists,
  Torrent (..),
  torrentByName,
  purgeTorrent,
  ActiveUser (..),
  getActiveUsers,
  DirectoryEntry (..),
  getDirectory,
  -- Model.Query
  QueryPage (..),
  -- Model.Stats
  StatsValue (..),
  getCounter,
  getDownloadCounter,
  addCounter,
  getGauge,
  setGauge,
  -- Model.Download
  InfoHash (..),
  infoHashToHex,
  Download (..),
  recentDownloads,
  mostDownloaded,
  userDownloads,
  enclosureDownloads,
  guidDownloads,
  torrentGuids,
  torrentDownloads,
  feedDownloads,
  searchDownloads,
  -- Model.Item
  Item (..),
  groupDownloads,
  userItemImage,
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
  enclosureErrors,
  FeedInfo (..),
  userFeed,
  userFeeds,
  userFeedInfo,
  addUserFeed,
  deleteUserFeed,
  feedByUrl,
  FeedDetails (..),
  userFeedDetails,
  setUserFeedDetails,
  searchFeeds,
  -- Model.Token
  Token (..),
  generateToken,
  validateToken,
  peekToken
  ) where

import Prelude
import Data.Convertible
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Typeable)
import qualified Data.ByteString as B
import Database.PostgreSQL.LibPQ (Connection)

import Utils
import Model.SqlValue
import Model.Query
import Model.Download
import Model.Item
import Model.Feed
import Model.User
import Model.Token
import Model.Stats                        
import PathPieces (DirectoryPage (..))


infoHashByName :: UserName -> Text -> Text -> Query InfoHash
infoHashByName user slug name =
  query "SELECT \"info_hash\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [convert user, convert slug, convert name]

data Torrent = Torrent {
  torrentInfoHash :: InfoHash,
  torrentName :: Text,
  torrentSize :: Integer,
  torrentTorrent :: B.ByteString
} deriving (Show, Typeable)

instance Convertible [SqlValue] Torrent where
  safeConvert [info_hash, name, size, torrent] =
    Torrent <$>
    safeConvert info_hash <*>
    safeConvert name <*>
    safeConvert size <*>
    safeConvert torrent
  safeConvert vals = convError "Torrent" vals

newtype InfoHashExists = InfoHashExists Bool

instance Convertible [SqlValue] InfoHashExists where
  safeConvert (value:_) =
    InfoHashExists <$>
    safeConvert value
  safeConvert _ =
    Right $
    InfoHashExists False

infoHashExists :: InfoHash -> Connection -> IO Bool
infoHashExists infoHash conn = do
  results <- infoHashExists' infoHash conn
  case results of
    (InfoHashExists True : _) -> return True
    _ -> return False

infoHashExists' :: InfoHash -> Query InfoHashExists
infoHashExists' infoHash =
  query "SELECT EXISTS (SELECT \"info_hash\" FROM torrents WHERE \"info_hash\"=?)" [convert infoHash]

torrentByName :: UserName -> Text -> Text -> Query Torrent
torrentByName user slug name =
  query "SELECT \"info_hash\", \"name\", \"size\", \"torrent\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [convert user, convert slug, convert name]

purgeTorrent :: UserName -> Text -> Text -> Connection -> IO Int
purgeTorrent user slug name db =
  head <$>
  query1 "SELECT * FROM purge_download(?, ?, ?)"
  [convert user, convert slug, convert name]
  db

data ActiveUser = ActiveUser
    { activeUser :: UserName
    , activeFeeds :: Int
    , activeFeedLangs :: Text
    , activeFeedTypes :: Text
    } deriving (Show, Typeable)
    
instance Convertible [SqlValue] ActiveUser where
    safeConvert [userVal, feedsVal, langsVal, typesVal] =
        ActiveUser <$>
        safeConvert userVal <*>
        safeConvert feedsVal <*>
        safeConvert langsVal <*>
        safeConvert typesVal
    safeConvert vals = convError "ActiveUser" vals
    
getActiveUsers :: Query ActiveUser 
getActiveUsers =
  query "SELECT \"user\", \"feeds\", array_to_string(\"langs\", ','), array_to_string(\"types\", ',') FROM active_users" []
    
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
    safeConvert [userVal, userTitleVal, userImageVal,
                 feedSlugVal, feedTitleVal, feedLangVal, feedTypesVal] =
      DirectoryEntry <$>
      safeConvert userVal <*>
      safeConvert userTitleVal <*>
      (fixUrl <$> safeConvert userImageVal) <*>
      safeConvert feedSlugVal <*>
      safeConvert feedTitleVal <*>
      safeConvert feedLangVal <*>
      safeConvert feedTypesVal
    safeConvert vals = convError "DirectoryEntry" vals
      
getDirectory :: Maybe DirectoryPage -> Query DirectoryEntry
getDirectory mPage =
  let (cond, params) =
          case mPage of
            Nothing ->
                ("", [])
            Just DirectoryDigit -> 
                (" WHERE \"user\" ~* E'^[^a-z]'", [])
            Just (DirectoryLetter c) ->
                (" WHERE LOWER(LEFT(\"user\", 1)) = ?", [convert $ T.pack [c]])
  in query ("SELECT \"user\", COALESCE(\"title\", \"user\"), COALESCE(\"image\", ''), \"slug\", COALESCE(\"feed_title\", \"slug\"), COALESCE(\"lang\", ''), array_to_string(\"types\", ',') FROM directory" ++ cond) params

