{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model (
  -- Model
  infoHashByName,
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
import Database.HDBC
import Data.Data (Typeable)
import qualified Data.ByteString.Lazy as LB
import Control.Applicative

import Utils
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
  query "SELECT \"info_hash\" FROM user_feeds JOIN enclosures USING (feed) JOIN enclosure_torrents USING (url) JOIN torrents USING (info_hash) WHERE user_feeds.\"user\"=? AND user_feeds.\"slug\"=? AND torrents.\"name\"=?" [toSql user, toSql slug, toSql name]

data Torrent = Torrent {
  torrentInfoHash :: InfoHash,
  torrentName :: Text,
  torrentSize :: Integer,
  torrentTorrent :: LB.ByteString
} deriving (Show, Typeable)

instance Convertible [SqlValue] Torrent where
  safeConvert (info_hash:name:size:torrent:[]) =
    Torrent <$>
    safeFromSql info_hash <*>
    safeFromSql name <*>
    safeFromSql size <*>
    pure (fromBytea torrent)
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

data ActiveUser = ActiveUser
    { activeUser :: UserName
    , activeFeeds :: Int
    , activeFeedLangs :: Text
    , activeFeedTypes :: Text
    } deriving (Show, Typeable)
    
instance Convertible [SqlValue] ActiveUser where
    safeConvert (userVal:feedsVal:langsVal:typesVal:[]) =
        ActiveUser <$>
        safeFromSql userVal <*>
        safeFromSql feedsVal <*>
        safeFromSql langsVal <*>
        safeFromSql typesVal
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
    safeConvert (userVal:userTitleVal:userImageVal:
                 feedSlugVal:feedTitleVal:feedLangVal:feedTypesVal:[]) = 
      DirectoryEntry <$>
      safeFromSql userVal <*>
      safeFromSql userTitleVal <*>
      (fixUrl <$> safeFromSql userImageVal) <*>
      safeFromSql feedSlugVal <*>
      safeFromSql feedTitleVal <*>
      safeFromSql feedLangVal <*>
      safeFromSql feedTypesVal
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
                (" WHERE LOWER(LEFT(\"user\", 1)) = ?", [toSql [c]])
  in query ("SELECT \"user\", COALESCE(\"title\", \"user\"), COALESCE(\"image\", ''), \"slug\", COALESCE(\"feed_title\", \"slug\"), COALESCE(\"lang\", ''), array_to_string(\"types\", ',') FROM directory" ++ cond) params

