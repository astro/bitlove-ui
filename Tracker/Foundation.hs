{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker.Foundation where

import Prelude
import Yesod
import qualified WorkQueue as WQ
import Model (InfoHash, infoHashExists)
import Model.Tracker (TrackerRequest(..), ScrapeInfo(..), scrape)
import Tracker.WebsocketHub

import Foundation (DBPool, HasDB(..), withDB, Transaction)
import Cache

data TrackerApp = TrackerApp
    { trackerDBPool :: DBPool
    , trackerCache :: Cache
    , trackerAnnounceQueue :: WQ.Queue
    , trackerScrapeQueue :: WQ.Queue
    , trackerHub :: Hub
    }

getCache :: HandlerT TrackerApp IO Cache
getCache = trackerCache <$> getYesod

checkExists :: TrackerRequest -> HandlerT TrackerApp IO Bool
checkExists tr = do
  let infoHash = trInfoHash tr
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
  |]

instance Yesod TrackerApp where
    makeSessionBackend _ = return Nothing

instance HasDB TrackerApp where
    getDBPool = trackerDBPool <$> getYesod

safeScrape :: Model.InfoHash -> Transaction ScrapeInfo
safeScrape infoHash db =
    (head . (++ [ScrapeInfo 0 0 0 0])) <$>
    scrape infoHash db
