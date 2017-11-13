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

type TrackedScrape = ()  -- TODO
data TrackedData = TrackedData { dataPeers :: HM.HashMap PeerId TrackedPeer
                               --, dataScrape :: TrackedScrape
                               }

updateData :: (HM.HashMap PeerId TrackedPeer -> HM.HashMap PeerId TrackedPeer) -> TrackedData -> TrackedData
updateData f (TrackedData { dataPeers = peers }) =
    TrackedData $
    f peers

newData :: TrackedData
newData =
  TrackedData { dataPeers = HM.empty
              }


newtype Tracked = Tracked (TVar (HM.HashMap InfoHash (TVar TrackedData)))

-- TODO: start clean loop
newTracked :: IO Tracked
newTracked =
  Tracked <$>
  newTVarIO HM.empty

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

-- TODO: must return counter events
announce :: Tracked -> TrackedAnnounce -> IO [(PeerId, TrackedPeer)]
announce tracked announce
  | aEvent announce == Just "stopped" = do
      trackedModifyData tracked (aInfoHash announce) $
        updateData $
        HM.delete $ aPeerId announce
      return []

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
                   if pLeft peer > 0
                   -- Return only leechers to seeders
                   then True
                   else False
                 | otherwise =
                   -- Return any valid peer to seeders
                   True
           in if emit
              then (peerId, peer) : peers
              else peers
        )
        [] (dataPeers data'')
    in (peers, data'')

-- TODO
announce _ _ = undefined


data ScrapeInfo = ScrapeInfo { scrapeLeechers :: Int
                             , scrapeSeeders :: Int
                             , scrapeUpspeed :: Int
                             , scrapeDownspeed :: Int
                             , scrapeDownloaded :: Int
                             } deriving (Show, Typeable)

scrape :: Tracked -> InfoHash -> IO ScrapeInfo
scrape tracked infoHash =
  HM.foldl' add newScrape <$>
  dataPeers <$>
  trackedGetData tracked infoHash

  where
    newScrape =
      ScrapeInfo { scrapeLeechers = 0
                 , scrapeSeeders = 0
                 , scrapeUpspeed = 0
                 , scrapeDownspeed = 0
                 , scrapeDownloaded = 0
                 }
    add :: ScrapeInfo -> TrackedPeer -> ScrapeInfo
    add si peer = si -- TODO

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
