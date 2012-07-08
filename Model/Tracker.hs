module Model.Tracker where

import Data.Convertible

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


getPeers :: InfoHash -> Bool -> Query TrackedPeer
getPeers infoHash onlyLeechers =
  query "SELECT \"peer_id\", \"host\", \"port\" FROM " ++
	    (if onlyLeechers 
             then "tracker_leechers"
             else "tracker"
            ) ++
	    " WHERE \"info_hash\"=? LIMIT 40"
            [toSql info_hash]
            

data TrackerRequest = TrackerRequest {
      trInfoHash :: Model.InfoHash,
      trPeerId :: BC.ByteString,
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
	    [toSql $ trInfoHash tr, toSql $ trPeerId tr]
      _ ->
          run db "SELECT * FROM set_peer(?, ?, ?, ?, ?, ?, ?)" $
          map (\f -> toSql $ f tr)
	  [trInfoHash, trHost, trPort, trPeerId, 
           trUploaded, trDownloaded, trLeft]

