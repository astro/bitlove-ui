{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Tracker where

import Prelude
import Control.Monad (void)
import Data.Convertible
import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Database.PostgreSQL.LibPQ (Connection)

import Model.SqlValue
import Model.Query
import Model.Download

data ScrapeInfo = ScrapeInfo {
      scrapeLeechers :: Integer,
      scrapeSeeders :: Integer,
      scrapeDownspeed :: Integer,
      scrapeDownloaded :: Integer
    } deriving (Show, Typeable)

instance Convertible [SqlValue] ScrapeInfo where
  safeConvert [leechers, seeders, downspeed, downloaded] =
    ScrapeInfo <$>
    safeConvert leechers <*>
    safeConvert seeders <*>
    safeConvert downspeed <*>
    safeConvert downloaded
  safeConvert vals = convError "ScrapeInfo" vals

scrape :: InfoHash -> Query ScrapeInfo
scrape infoHash =
  query "SELECT \"leechers\", \"seeders\", \"downspeed\", COALESCE(\"downloaded\", 0) FROM scraped LEFT JOIN downloaded_stats USING (info_hash) WHERE \"info_hash\"=?"
  [convert infoHash]



newtype PeerId = PeerId { unPeerId :: BC.ByteString }
               deriving (Show, Typeable)

instance Convertible SqlValue PeerId where
    safeConvert = (PeerId <$>) . safeConvert

instance Convertible PeerId SqlValue where
    safeConvert = safeConvert . unPeerId

data PeerAddress = Peer4 !BC.ByteString
                 | Peer6 !BC.ByteString
                   deriving (Show, Read, Typeable, Eq, Ord)

instance Convertible PeerAddress SqlValue where
    safeConvert (Peer4 addr) = safeConvert addr
    safeConvert (Peer6 addr) = safeConvert addr

instance Convertible SqlValue PeerAddress where
    safeConvert v = safeConvert v >>= decide
        where decide addr
                  | BC.length addr == 4 =
                      Right $ Peer4 addr
                  | BC.length addr == 16 =
                      Right $ Peer6 addr
                  | otherwise =
                      convError "PeerAddress" addr

data TrackedPeer = TrackedPeer !PeerId !PeerAddress !Int
                   deriving (Show, Typeable)

instance Convertible [SqlValue] TrackedPeer where
    safeConvert [peerId, addr, port] =
        TrackedPeer <$>
        safeConvert peerId <*>
        safeConvert addr <*>
        safeConvert port
    safeConvert vals =
        convError "TrackedPeer" vals

getPeers :: InfoHash -> Bool -> Query TrackedPeer
getPeers infoHash onlyLeechers =
    query ("SELECT \"peer_id\", \"host\", \"port\" FROM " ++
           (if onlyLeechers
            then "tracker_leechers"
            else "tracker"
           ) ++
           " WHERE \"info_hash\"=? AND \"offers\" IS NULL LIMIT 40")
              [convert infoHash]

data TrackedWebPeer = TrackedWebPeer !PeerId LBC.ByteString
                   deriving (Show, Typeable)

instance Convertible [SqlValue] TrackedWebPeer where
    safeConvert [peerId, offers] =
        TrackedWebPeer <$>
        safeConvert peerId <*>
        safeConvert offers
    safeConvert vals =
        convError "TrackedWebPeer" vals

getWebPeers :: InfoHash -> Bool -> Query TrackedWebPeer
getWebPeers infoHash onlyLeechers =
    query ("SELECT \"peer_id\", \"offers\" FROM " ++
           (if onlyLeechers
            then "tracker_leechers"
            else "tracker"
           ) ++
           " WHERE \"info_hash\"=? AND \"offers\" IS NOT NULL LIMIT 40")
              [convert infoHash]


data TrackerRequest = TrackerRequest {
      trInfoHash :: InfoHash,
      trPeerId :: PeerId,
      trHost :: PeerAddress,
      trPort :: Int,
      trUploaded :: Integer,
      trDownloaded :: Integer,
      trLeft :: Integer,
      trEvent :: Maybe Text,
      trCompact :: Bool,
      trOffers :: Maybe LBC.ByteString
    } deriving (Show)

announcePeer :: TrackerRequest -> Connection -> IO ()
announcePeer tr db =
    case trEvent tr of
      Just "stopped" ->
          void $
          query' "DELETE FROM tracked WHERE \"info_hash\"=? AND \"peer_id\"=? RETURNING \"uploaded\", \"downloaded\""
            [convert $ trInfoHash tr, convert $ trPeerId tr] db
      _ ->
          do let m :: Convertible a SqlValue => (TrackerRequest -> a) -> SqlValue
                 m = convert . ($ tr)
             void $
                 query' "SELECT * FROM set_peer(?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 [m trInfoHash, m trHost, m trPort, m trPeerId,
                  m trUploaded, m trDownloaded, m trLeft, m trEvent,
                  m trOffers]
                 db

updateScraped :: InfoHash -> Connection -> IO ()
updateScraped infoHash db =
    void $
    query' "SELECT * FROM update_scraped(?)" [convert infoHash] db
