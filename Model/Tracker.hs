{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Model.Tracker where

import Prelude
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
  safeConvert (leechers:seeders:downspeed:downloaded:[]) =
    Right $
    ScrapeInfo
    (fromSql leechers)
    (fromSql seeders)
    (fromSql downspeed)
    (fromSql downloaded)
  safeConvert vals = convError "ScrapeInfo" vals

scrape :: InfoHash -> Query ScrapeInfo
scrape infoHash =
  query "SELECT \"leechers\", \"seeders\", \"downspeed\", COALESCE(\"downloaded\", 0) FROM scraped LEFT JOIN downloaded_stats USING (info_hash) WHERE \"info_hash\"=?"
  [toSql infoHash]


data PeerAddress = Peer4 BC.ByteString
                 | Peer6 BC.ByteString
                   deriving (Show, Read, Typeable, Eq, Ord)
                   
instance Convertible PeerAddress SqlValue where
    safeConvert (Peer4 addr) = Right $ toBytea addr
    safeConvert (Peer6 addr) = Right $ toBytea addr

instance Convertible SqlValue PeerAddress where
    safeConvert = decide . fromBytea
        where decide addr
                  | BC.length addr == 4 =
                      Right $ Peer4 addr
                  | BC.length addr == 16 =
                      Right $ Peer6 addr
                  | otherwise =
                      convError "PeerAddress" addr

data TrackedPeer = TrackedPeer BC.ByteString PeerAddress Int
                   deriving (Show, Typeable)
                            
instance Convertible [SqlValue] TrackedPeer where
    safeConvert (peerId:addr:port:[]) =
        Right $
        TrackedPeer
        (fromBytea peerId)
        (fromSql addr)
        (fromSql port)
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
      trPeerId :: BC.ByteString,
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
          run db "DELETE FROM tracked WHERE \"info_hash\"=? AND \"peer_id\"=? RETURNING \"uploaded\", \"downloaded\""
	    [toSql $ trInfoHash tr, toBytea $ trPeerId tr]
          >>
          return ()
      _ ->
          do let m :: Convertible a SqlValue => (TrackerRequest -> a) -> SqlValue
                 m = toSql . ($ tr)
             _ <-
                 run db "SELECT * FROM set_peer(?, ?, ?, ?, ?, ?, ?)" $
                 [m trInfoHash, m trHost, m trPort, toBytea $ trPeerId tr, 
                  m trUploaded, m trDownloaded, m trLeft]
             return ()
