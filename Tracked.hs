module Tracked where

import Prelude
import Data.Hashable (Hashable)
import Data.Data (Typeable)
import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BC

import Utils (getNow)
import Model (InfoHash)

newtype PeerId = PeerId { unPeerId :: BC.ByteString }
               deriving (Show, Typeable, Eq, Hashable)

data PeerAddress = Peer4 !BC.ByteString
                 | Peer6 !BC.ByteString
                   deriving (Show, Read, Typeable, Eq, Ord)

data TrackedPeer = BittorrentPeer { pBtHost :: !PeerAddress
                                  , pBtPort :: !Int
                                  , pUploaded :: !Int
                                  , pDownloaded :: !Int
                                  , pUpspeed :: !Int
                                  , pDownspeed :: !Int
                                  , pLeft :: !Int
                                  , pLastRequest :: !Int
                                  }
                 | WebtorrentPeer { pWtOffers :: [()]
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

trackedGetData :: Tracked -> InfoHash -> IO (Maybe TrackedData)
trackedGetData (Tracked trackedRef) infoHash =
  atomically $
  HM.lookup infoHash <$>
  readTVar trackedRef >>=
  maybe (return Nothing) ((Just <$>) . readTVar)

trackedModifyData :: Tracked -> InfoHash -> (TrackedData -> TrackedData) -> IO ()
trackedModifyData (Tracked trackedRef) infoHash f =
  atomically $ do
  tracked <- readTVar trackedRef
  let mDataRef = HM.lookup infoHash tracked
  d <- maybe (return newData) readTVar mDataRef

  let d' = f d

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


data TrackedAnnounce
  = BittorrentAnnounce { aInfoHash :: !InfoHash
                       , aPeerId :: !PeerId
                       , aHost :: !PeerAddress
                       , aPort :: !Int
                       , aUploaded :: !Int
                       , aDownloaded :: !Int
                       , aLeft :: !Int
                       , aEvent :: Maybe Text
                       , aCompact :: !Bool
                       }
  | WebtorrentAnnounce { aInfoHash :: !InfoHash
                       , aPeerId :: !PeerId
                       , aOffers :: [()]
                       , aUploaded :: !Int
                       , aDownloaded :: !Int
                       , aLeft :: !Int
                       , aEvent :: Maybe Text
                       , aCompact :: !Bool
                       } deriving (Show)

-- TODO: must return counter events
announce :: Tracked -> TrackedAnnounce -> IO ()
announce tracked announce
  | aEvent announce == Just "stopped" =
    trackedModifyData tracked (aInfoHash announce) $
      updateData $
      HM.delete $ aPeerId announce

announce tracked announce@(WebtorrentAnnounce {}) = do
  now <- getNow
  let newPeer =
        WebtorrentPeer { pWtOffers = aOffers announce
                       , pUploaded = aUploaded announce
                       , pDownloaded = aDownloaded announce
                       , pUpspeed = 0
                       , pDownspeed = 0
                       , pLeft = aLeft announce
                       , pLastRequest = now
                       }
  trackedModifyData tracked (aInfoHash announce) $
    updateData $
    HM.alter
    (maybe (Just newPeer)
     (\oldPeer ->
        Just $! updatePeerDeltas oldPeer newPeer
     ))
    (aPeerId announce)
  
-- TODO
announce _ _ = undefined
