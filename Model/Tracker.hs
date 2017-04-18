{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Tracker where

import Prelude
import Control.Monad (void)
import Data.Convertible
import Data.Data (Typeable)
import Database.HDBC
import qualified Data.ByteString.Char8 as BC

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
    safeFromSql leechers <*>
    safeFromSql seeders <*>
    safeFromSql downspeed <*>
    safeFromSql downloaded
  safeConvert vals = convError "ScrapeInfo" vals

scrape :: InfoHash -> Query ScrapeInfo
scrape infoHash =
  query "SELECT \"leechers\", \"seeders\", \"downspeed\", COALESCE(\"downloaded\", 0) FROM scraped LEFT JOIN downloaded_stats USING (info_hash) WHERE \"info_hash\"=?"
  [toSql infoHash]



newtype PeerId = PeerId { unPeerId :: BC.ByteString }
               deriving (Show, Typeable)

instance Convertible SqlValue PeerId where
    safeConvert = Right . PeerId . fromBytea'

instance Convertible PeerId SqlValue where
    safeConvert = Right . toBytea' . unPeerId

data PeerAddress = Peer4 !BC.ByteString
                 | Peer6 !BC.ByteString
                   deriving (Show, Read, Typeable, Eq, Ord)

instance Convertible PeerAddress SqlValue where
    safeConvert (Peer4 addr) = Right $ toBytea' addr
    safeConvert (Peer6 addr) = Right $ toBytea' addr

instance Convertible SqlValue PeerAddress where
    safeConvert = decide . fromBytea'
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
        safeFromSql peerId <*>
        safeFromSql addr <*>
        safeFromSql port
    safeConvert vals =
        convError "TrackedPeer" vals

getPeers :: InfoHash -> Bool -> Query TrackedPeer
getPeers infoHash onlyLeechers =
    query ("SELECT \"peer_id\", \"host\", \"port\" FROM " ++
           (if onlyLeechers
            then "tracker_leechers"
            else "tracker"
           ) ++
           " WHERE \"info_hash\"=? LIMIT 40")
              [toSql infoHash]


data TrackerRequest = TrackerRequest {
      trInfoHash :: InfoHash,
      trPeerId :: PeerId,
      trHost :: PeerAddress,
      trPort :: Int,
      trUploaded :: Integer,
      trDownloaded :: Integer,
      trLeft :: Integer,
      trEvent :: Maybe BC.ByteString,
      trCompact :: Bool
    } deriving (Show)

announcePeer :: IConnection conn =>
                TrackerRequest -> conn -> IO ()
announcePeer tr db =
    case trEvent tr of
      Just "stopped" ->
          void $
          run db "DELETE FROM tracked WHERE \"info_hash\"=? AND \"peer_id\"=? RETURNING \"uploaded\", \"downloaded\""
            [toSql $ trInfoHash tr, toSql $ trPeerId tr]
      _ ->
          do let m :: Convertible a SqlValue => (TrackerRequest -> a) -> SqlValue
                 m = toSql . ($ tr)
             void $
                 run db "SELECT * FROM set_peer(?, ?, ?, ?, ?, ?, ?, ?)"
                 [m trInfoHash, m trHost, m trPort, m trPeerId,
                  m trUploaded, m trDownloaded, m trLeft, m trEvent]

updateScraped :: IConnection conn =>
                 InfoHash -> conn -> IO ()
updateScraped infoHash db =
    void $
    run db "SELECT * FROM update_scraped(?)" [toSql infoHash]
