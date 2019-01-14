{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker.Foundation where

import Prelude
import Yesod
import Yesod.Default.Config

import Model (InfoHash, infoHashExists)
import Foundation (DBPool, HasDB(..), withDB, BitloveEnv, Extra)
import Cache
import Tracked (Tracked)

data TrackerApp = TrackerApp
    { settings :: AppConfig BitloveEnv Extra
    , trackerDBPool :: DBPool
    , trackerCache :: Cache
    , trackerTracked :: Tracked
    }

getCache :: HandlerFor TrackerApp Cache
getCache = trackerCache <$> getYesod

checkExists :: InfoHash -> HandlerFor TrackerApp Bool
checkExists infoHash = do
  cachedExists <- getCache >>=
                  liftIO . getTorrentExists infoHash
  case cachedExists of
    False ->
      return False
    True -> do
      torrentExists <- withDB $ infoHashExists infoHash
      case torrentExists of
        True ->
          return True
        False ->
          getCache >>=
          liftIO . setTorrentExists infoHash False >>
          return False

mkYesodData "TrackerApp"
  [parseRoutes|
    /announce AnnounceR GET
    /scrape ScrapeR GET
    /webtorrent-announce WebTorrentAnnounce GET
    /announce/export ExportStream GET
  |]

instance Yesod TrackerApp where
    makeSessionBackend _ = return Nothing

instance HasDB TrackerApp where
    getDBPool = trackerDBPool <$> getYesod
