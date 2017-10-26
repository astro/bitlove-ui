{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker.Foundation where

import Prelude
import Yesod
import qualified WorkQueue as WQ

import Foundation (DBPool, HasDB(..))
import Cache

data TrackerApp = TrackerApp
    { trackerDBPool :: DBPool
    , trackerCache :: Cache
    , trackerAnnounceQueue :: WQ.Queue
    , trackerScrapeQueue :: WQ.Queue
    }

getCache :: HandlerT TrackerApp IO Cache
getCache = trackerCache <$> getYesod

mkYesodData "TrackerApp"
  [parseRoutes|
    /announce AnnounceR GET
    /scrape ScrapeR GET
    /webtorrent-announce WebTorrentAnnounce GET
  |]

instance Yesod TrackerApp where
    makeSessionBackend _ = return Nothing

instance HasDB TrackerApp where
    getDBPool = trackerDBPool <$> getYesod
