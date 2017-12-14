module Tracked where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)
import Data.Data (Typeable)
import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (Value)

import Utils (getNow)
import Model (InfoHash)

newtype PeerId = PeerId { unPeerId :: BC.ByteString }
               deriving (Show, Typeable, Eq, Ord, Hashable)

data PeerAddress = Peer4 !BC.ByteString
                 | Peer6 !BC.ByteString
                   deriving (Show, Read, Typeable, Eq, Ord)

data ConnInfo = BittorrentInfo !PeerAddress !Int
              | WebtorrentInfo (TChan Value)

data TrackedPeer = TrackedPeer { pConnInfo :: !ConnInfo
                               , pUploaded :: !Int
                               , pDownloaded :: !Int
                               , pUpspeed :: !Int
                               , pDownspeed :: !Int
                               , pLeft :: !Int
                               , pLastRequest :: !Int
                               }

updatePeerDeltas :: TrackedPeer -> TrackedPeer -> TrackedPeer
updatePeerDeltas oldPeer newPeer =
  newPeer { pUpspeed = max 0 $
                       (pUploaded newPeer - pUploaded oldPeer) `div` dt
          , pDownspeed = max 0 $
                       (pDownloaded newPeer - pDownloaded oldPeer) `div` dt
          }
  where dt = max 1 $
             pLastRequest newPeer - pLastRequest oldPeer

data TrackedScrape = TrackedScrape {
  scrapeLeechers :: !Int,
  scrapeSeeders :: !Int,
  scrapeDownspeed :: !Int,
  scrapeUpspeed :: !Int
} deriving (Show, Typeable)

instance Monoid TrackedScrape where
  mempty = TrackedScrape 0 0 0 0
  (TrackedScrape l1 s1 d1 u1) `mappend` (TrackedScrape l2 s2 d2 u2) =
    TrackedScrape (l1 + l2) (s1 + s2) (d1 + d2) (u1 + u2)

data TrackedData = TrackedData { dataPeers :: HM.HashMap PeerId TrackedPeer
                               , dataBittorrentScrape :: TrackedScrape
                               , dataWebtorrentScrape :: TrackedScrape
                               }

updateData :: (HM.HashMap PeerId TrackedPeer -> HM.HashMap PeerId TrackedPeer) -> TrackedData -> TrackedData
updateData f (TrackedData { dataPeers = peers }) =
  let peers' = f peers
      scrape = HM.foldl'
               (\scrape peer ->
                 let isSeeder =
                       pLeft peer == 0
                     scrape' =
                       TrackedScrape
                       { scrapeLeechers =
                           if isSeeder
                           then 0
                           else 1
                       , scrapeSeeders =
                           if isSeeder
                           then 1
                           else 0
                       , scrapeDownspeed =
                           pDownspeed peer
                       , scrapeUpspeed =
                           pUploaded peer
                       }
                 in scrape `mappend` scrape'
               ) mempty peers'
  in TrackedData peers' scrape scrape

newData :: TrackedData
newData =
  TrackedData { dataPeers = HM.empty
              , dataBittorrentScrape = mempty
              , dataWebtorrentScrape = mempty
              }

newtype Tracked = Tracked (TVar (HM.HashMap InfoHash (TVar TrackedData)))

newTracked :: IO Tracked
newTracked = do
  peers <- newTVarIO HM.empty

  -- Assume a single instance, clear all gauges of a previous crash
  -- TODO

  -- TODO: forkIO clean loop

  return $ Tracked peers

trackedGetData :: Tracked -> InfoHash -> IO TrackedData
trackedGetData (Tracked trackedRef) infoHash =
  atomically $
  HM.lookup infoHash <$>
  readTVar trackedRef >>=
  maybe (return newData) readTVar

trackedModifyData' :: Tracked -> InfoHash -> (TrackedData -> (a, TrackedData)) -> IO a
trackedModifyData' (Tracked trackedRef) infoHash f =
  atomically $ do
  tracked <- readTVar trackedRef
  let mDataRef = HM.lookup infoHash tracked
  d <- maybe (return newData) readTVar mDataRef

  let (a, d') = f d

  case HM.null $ dataPeers d' of
    True ->
      case mDataRef of
        -- infoHash was there before but has no more peers now
        Just _ ->
          writeTVar trackedRef $
          HM.delete infoHash tracked
        -- infoHash wasn't there before and has no peers now
        Nothing ->
          return ()
    False ->
      case mDataRef of
        -- infoHash wasn't there before and has peers now
        Nothing -> do
          dataRef <- newTVar d'
          writeTVar trackedRef $
            HM.insert infoHash dataRef tracked
        -- infoHash was there before but has no more peers now
        Just dataRef ->
          writeTVar dataRef d'

  return a

trackedModifyData :: Tracked -> InfoHash -> (TrackedData -> TrackedData) -> IO ()
trackedModifyData tracked infoHash f =
  trackedModifyData' tracked infoHash $
  ((), ) . f


data TrackedAnnounce
  = TrackedAnnounce { aInfoHash :: !InfoHash
                    , aPeerId :: !PeerId
                    , aConnInfo :: !ConnInfo
                    , aUploaded :: !Int
                    , aDownloaded :: !Int
                    , aLeft :: !Int
                    , aEvent :: Maybe Text
                    }

data TrackedAnnounced
  = TrackedAnnounced { adPeers :: [(PeerId, TrackedPeer)]
                     , adCompleted :: Bool
                     , adUploaded :: Int
                     , adDownloaded :: Int
                     }

-- TODO: must return counter events
announce :: Tracked -> TrackedAnnounce -> IO TrackedAnnounced
announce tracked announce
  | aEvent announce == Just "stopped" = do
      trackedModifyData tracked (aInfoHash announce) $
        updateData $
        HM.delete $ aPeerId announce
      return $ TrackedAnnounced [] False 0 0

announce tracked announce@(TrackedAnnounce {}) = do
  now <- getNow
  let isSeeder = aLeft announce == 0
      newPeer =
        TrackedPeer { pUploaded = aUploaded announce
                    , pDownloaded = aDownloaded announce
                    , pUpspeed = 0
                    , pDownspeed = 0
                    , pLeft = aLeft announce
                    , pLastRequest = now
                    , pConnInfo = aConnInfo announce
                    }
      -- | For filtering result peers
      isEqInfo :: TrackedPeer -> Bool
      isEqInfo peer =
        case (aConnInfo announce, pConnInfo peer) of
          (BittorrentInfo {}, BittorrentInfo {}) ->
            True
          (WebtorrentInfo {}, WebtorrentInfo {}) ->
            True
          _ ->
            False
  trackedModifyData' tracked (aInfoHash announce) $ \data' ->
    let
      oldPeer =
        aPeerId announce `HM.lookup` dataPeers data'
      data'' =
        updateData
        (HM.alter
         (maybe (Just newPeer)
          (\oldPeer ->
             Just $! updatePeerDeltas oldPeer newPeer
          ))
         (aPeerId announce)
        ) data'
      peers =
        HM.foldlWithKey'
        (\peers peerId peer ->
           let emit
                 | peerId == aPeerId announce =
                     -- Don't return the same peer that requested
                     False
                 | not (isEqInfo peer) =
                     -- Don't return TCP peers to WebRTC peers and vice versa
                     False
                 | isSeeder =
                   -- Return only leechers to seeders
                   pLeft peer > 0
                 | otherwise =
                   -- Return any valid peer to seeders
                   True
           in if emit
              then (peerId, peer) : peers
              else peers
        )
        [] (dataPeers data'')
      completed =
        aEvent announce == Just "completed"
      uploaded =
        max 0 $
        fromMaybe 0 $ do
        oldPeer' <- oldPeer
        return $ pUploaded newPeer - pUploaded oldPeer'
      downloaded =
        max 0 $
        fromMaybe 0 $ do
        oldPeer' <- oldPeer
        return $ pDownloaded newPeer - pDownloaded oldPeer'
      ad =
        TrackedAnnounced { adPeers = peers
                         , adCompleted = completed
                         , adUploaded = uploaded
                         , adDownloaded = downloaded
                         }
    in (ad, data'')

-- TODO
announce _ _ = error "announce not implemented yet"

scrapeWebtorrent :: Tracked -> InfoHash -> IO TrackedScrape
scrapeWebtorrent tracked infoHash =
  dataWebtorrentScrape <$>
  trackedGetData tracked infoHash

scrapeBittorrent :: Tracked -> InfoHash -> IO TrackedScrape
scrapeBittorrent tracked infoHash =
  dataBittorrentScrape <$>
  trackedGetData tracked infoHash

getPeer :: Tracked -> InfoHash -> PeerId -> IO (Maybe TrackedPeer)
getPeer tracked infoHash peerId =
  HM.lookup peerId <$>
  dataPeers <$>
  trackedGetData tracked infoHash

clearPeer :: Tracked -> InfoHash -> PeerId -> IO ()
clearPeer tracked infoHash peerId =
  trackedModifyData tracked infoHash $
  updateData $
  HM.delete $ peerId
