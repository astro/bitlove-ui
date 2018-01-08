module Tracked where

import Prelude
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Data (Typeable)
import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (Value)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Numeric (showHex)
import Data.Bits (shiftL, (.|.))

import Utils (getNow)
import Model (InfoHash)

newtype PeerId = PeerId { unPeerId :: BC.ByteString }
               deriving (Show, Typeable, Eq, Ord, Hashable)

data PeerAddress = Peer4 !BC.ByteString
                 | Peer6 !BC.ByteString
                   deriving (Read, Typeable, Eq, Ord)

instance Show PeerAddress where
  show (Peer4 bs) =
    intercalate "." $
    map show $
    B.unpack bs
  show (Peer6 bs) =
    let groupAt :: Int -> [a] -> [[a]]
        groupAt n xs
          | length xs <= n =
              [xs]
          | otherwise =
              take n xs : groupAt n (drop n xs)

        words = groupAt 2 $ B.unpack bs

    in if length words == 8
       then intercalate ":" $
            map (\[b1, b2] ->
                   showHex ((b1 `shiftL` 8) .|. b2) ""
                ) words
       else "::"  -- ^invalid


instance Hashable PeerAddress where
  hashWithSalt salt (Peer4 bs) = hashWithSalt salt bs
  hashWithSalt salt (Peer6 bs) = hashWithSalt salt bs

data ConnInfo = BittorrentInfo !PeerAddress !Int
              | WebtorrentInfo !PeerAddress (TChan Value)

instance Show ConnInfo where
  show (BittorrentInfo addr port) = show addr ++ ":" ++ show port
  show (WebtorrentInfo addr _) = "<WS " ++ show addr ++ ">"

cKind :: ConnInfo -> TrackedKind
cKind (BittorrentInfo _ _) =
  Bittorrent
cKind (WebtorrentInfo _ _) =
  Webtorrent

cAddr :: ConnInfo -> PeerAddress
cAddr (BittorrentInfo addr _) = addr
cAddr (WebtorrentInfo addr _) = addr

data TrackedPeer = TrackedPeer { pConnInfo :: !ConnInfo
                               , pUploaded :: !Int
                               , pDownloaded :: !Int
                               , pUpspeed :: !Int
                               , pDownspeed :: !Int
                               , pLeft :: !Int
                               , pLastRequest :: !Int
                               } deriving (Show)

data TrackedKind = Bittorrent | Webtorrent
                 deriving (Eq, Show, Ord)

pKind :: TrackedPeer -> TrackedKind
pKind (TrackedPeer { pConnInfo = c }) =
  cKind c

pAddr :: TrackedPeer -> PeerAddress
pAddr (TrackedPeer { pConnInfo = c }) =
  cAddr c

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
      scrape kind =
        HM.foldl'
        (\scrape peer ->
            let isSeeder =
                  pLeft peer == 0
            in if pKind peer == kind
               then scrape `mappend`
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
               else scrape
        ) mempty peers'
  in TrackedData peers' (scrape Bittorrent) (scrape Webtorrent)

newData :: TrackedData
newData =
  TrackedData { dataPeers = HM.empty
              , dataBittorrentScrape = mempty
              , dataWebtorrentScrape = mempty
              }

data Tracked = Tracked
  { trackedTTracked :: TVar (HM.HashMap InfoHash (TVar TrackedData))
  , trackedTPopular :: TVar [InfoHash]
  }

trackedPopular :: Tracked -> IO [InfoHash]
trackedPopular = atomically . readTVar . trackedTPopular

newTracked :: IO Tracked
newTracked = do
  tTracked <- newTVarIO HM.empty
  tPopular <- newTVarIO []
  let tracked = Tracked tTracked tPopular

  let cleanAndStats = do
        t1 <- getCurrentTime
        infoHashes <- atomically $
                      HM.keys <$>
                      readTVar tTracked
        t2 <- getCurrentTime
        popular <-
          (map snd . Map.toDescList) <$>
          foldM (\popular infoHash -> do
                    now <- getNow
                    trackedModifyData' tracked infoHash $ \d ->
                      let d' =
                            -- Drop outdated peers
                            updateData (HM.filter $
                                        \peer ->
                                          now <= pLastRequest peer + peerTimeout
                                       ) d
                          -- |Collect popular list
                          scrape =
                            dataBittorrentScrape d' `mappend`
                            dataWebtorrentScrape d'
                          popularity =
                            (scrapeSeeders scrape +
                             scrapeLeechers scrape,
                             scrapeSeeders scrape,
                             infoHash
                            )
                          popular' =
                            Map.insert popularity infoHash popular
                          popular''
                            | Map.size popular < popularTorrents =
                                -- Need more popularTorrents
                                popular'
                            | otherwise =
                                -- Drop one off
                                let lowestPopularity =
                                      fst $ head $
                                      Map.toAscList popular'
                                in Map.delete lowestPopularity popular'
                      in (popular'', d')
                ) Map.empty infoHashes
        t3 <- getCurrentTime
        atomically $
          writeTVar tPopular popular

        t4 <- getCurrentTime
        putStrLn $ "Found " ++
          show (length popular) ++ " popular torrents from " ++
          show (length infoHashes) ++ " info_hashes in " ++
          show (truncate $ (t2 `diffUTCTime` t1) * 1000 :: Int) ++ "+" ++
          show (truncate $ (t3 `diffUTCTime` t2) * 1000 :: Int) ++ "+" ++
          show (truncate $ (t4 `diffUTCTime` t3) * 1000 :: Int) ++ "ms"

      handleException :: E.SomeException -> IO ()
      handleException = print

      cleanAndStatsLoop =
        forever $ do
        E.catch cleanAndStats handleException
        -- Sleep 10s before next run
        threadDelay 10000000

  forkIO cleanAndStatsLoop

  -- Assume a single instance, clear all gauges of a previous crash
  -- TODO

  return tracked

    where
      peerTimeout = 3600
      popularTorrents = 100

trackedGetData :: Tracked -> InfoHash -> IO TrackedData
trackedGetData (Tracked tTracked _) infoHash =
  atomically $
  HM.lookup infoHash <$>
  readTVar tTracked >>=
  maybe (return newData) readTVar

trackedModifyData' :: Tracked -> InfoHash -> (TrackedData -> (a, TrackedData)) -> IO a
trackedModifyData' (Tracked tTracked _) infoHash f =
  atomically $ do
  tracked <- readTVar tTracked
  let mDataRef = HM.lookup infoHash tracked
  d <- maybe (return newData) readTVar mDataRef

  let (a, d') = f d

  case HM.null $ dataPeers d' of
    True ->
      case mDataRef of
        -- infoHash was there before but has no more peers now
        Just _ ->
          writeTVar tTracked $
          HM.delete infoHash tracked
        -- infoHash wasn't there before and has no peers now
        Nothing ->
          return ()
    False ->
      case mDataRef of
        -- infoHash wasn't there before and has peers now
        Nothing -> do
          dataRef <- newTVar d'
          writeTVar tTracked $
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
                    } deriving (Show)

data TrackedAnnounced
  = TrackedAnnounced { adPeers :: [(PeerId, TrackedPeer)]
                     , adCompleted :: Bool
                     , adUploaded :: Int
                     , adDownloaded :: Int
                     } deriving (Show)

announce :: Tracked -> TrackedAnnounce -> IO TrackedAnnounced
announce tracked announce
  | aEvent announce == Just "stopped" = do
      let peerId = aPeerId announce
      (uploaded, downloaded) <- trackedModifyData' tracked (aInfoHash announce) $ \d ->
        case HM.lookup peerId (dataPeers d) of
          Nothing ->
            ((0, 0), d)
          Just peer ->
            let uploaded = aUploaded announce - pUploaded peer
                downloaded = aDownloaded announce - pDownloaded peer
                d' = updateData (HM.delete peerId) d
            in ((uploaded, downloaded), d')

      return $ TrackedAnnounced [] False uploaded downloaded

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
        pKind peer == cKind (aConnInfo announce)
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
        -- Count event=completed
        aEvent announce == Just "completed" ||
        -- Or when left=0 and it previously wasn't
        (aLeft announce == 0 && fromMaybe 0 (pLeft <$> oldPeer) > 0)
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

getChangedAddrs :: Tracked -> HashSet PeerAddress -> IO (HashSet PeerAddress, HashSet PeerAddress, HashSet PeerAddress)
getChangedAddrs tracked known =
  atomically $ do
  tracked' <- readTVar (trackedTTracked tracked)
  current <-
    foldM
    (\current tData ->
        foldl
        (\current peer ->
           HashSet.insert (pAddr peer) current
        ) current <$>
        dataPeers <$>
        readTVar tData
    ) HashSet.empty (HM.elems tracked')

  let
    -- | removed means it was known but is not current
    removed = known `HashSet.difference` current
    -- | added means it is there currently but was not known before
    added = current `HashSet.difference` known

  case (HashSet.null removed, HashSet.null added) of
    (True, True) ->
      -- No changes
      retry
    _ ->
      return (current, removed, added)
