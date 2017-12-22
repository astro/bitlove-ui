module Tracker.Handler.Webtorrent (getWebTorrentAnnounce) where

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Exception.Enclosed
import System.Random (randomIO, randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import Yesod.WebSockets
import Data.Aeson
import Data.IORef
import Control.Concurrent.STM

import Import hiding (Handler)
import Tracker.Foundation
import Tracker.Utils
import qualified Model as Model
import Tracked


getWebTorrentAnnounce :: Handler ()
getWebTorrentAnnounce = do
  webSockets runWebsocket
  invalidArgs ["WebSocket expected"]

data Offer = Offer { offerId :: Text
                   , offer :: Object
                   } deriving (Show)

instance FromJSON Offer where
  parseJSON (Object o) =
    Offer
    <$> o .: "offer_id"
    <*> o .: "offer"

data Message = AnnounceMessage {
  msgAction :: Maybe Text,
  msgInfoHash :: Maybe Text,
  msgPeerId :: Maybe Text,
  msgDownloaded :: Maybe Int,
  msgUploaded :: Maybe Int,
  msgLeft :: Maybe Int,
  msgEvent :: Maybe Text,
  msgNumWant :: Maybe Int,
  msgOffers :: Maybe [Offer],
  msgOfferId :: Maybe Text,
  msgToPeerId :: Maybe Text,
  msgAnswer :: Maybe Object
} deriving (Show)

instance FromJSON Message where
  parseJSON (Object o) = do
    action <- o .: "action"
    case action :: Text of
      "announce" ->
        AnnounceMessage
        <$> o .:? "action"
        <*> o .:? "info_hash"
        <*> o .:? "peer_id"
        <*> o .:? "downloaded"
        <*> o .:? "uploaded"
        <*> o .:? "left"
        <*> o .:? "event"
        <*> o .:? "numwant"
        <*> o .:? "offers"
        <*> o .:? "offer_id"
        <*> o .:? "to_peer_id"
        <*> o .:? "answer"
      _ ->
        fail $ "No supported action: " ++ show action
  parseJSON _ = fail "Not an object"

messageToAnnounce :: Message -> TChan Value -> Maybe TrackedAnnounce
messageToAnnounce msg@(AnnounceMessage {}) chan =
  TrackedAnnounce
  <$> Model.InfoHash <$> encodeLatin1 <$> msgInfoHash msg
  <*> (PeerId <$> encodeLatin1 <$> msgPeerId msg)
  <*> pure (WebtorrentInfo chan)
  <*> msgUploaded msg
  <*> msgDownloaded msg
  <*> msgLeft msg
  <*> pure (msgEvent msg)

data TrackerError = TrackerError Text

instance ToJSON TrackerError where
  toJSON (TrackerError reason) =
    object [ "action" .= ("announce" :: Text)
           , "failure reason" .= reason
           , "interval" .= (0xffff :: Int)
           ]

data TrackerResponse = TrackerResponse InfoHash TrackedScrape Int

instance ToJSON TrackerResponse where
  toJSON (TrackerResponse infoHash scrape interval) =
    object [ "action" .= ("announce" :: Text)
           , "info_hash" .= decodeLatin1 (unInfoHash infoHash)
           , "interval" .= interval
           , "complete" .= scrapeSeeders scrape
           , "incomplete" .= scrapeLeechers scrape
           , "downloaded" .= (0 :: Int)  -- TODO
           ]

data PeerOffer = PeerOffer InfoHash PeerId Offer

instance ToJSON PeerOffer where
  toJSON (PeerOffer infoHash peerId o) =
    object [ "action" .= ("announce" :: Text)
           , "info_hash" .= decodeLatin1 (unInfoHash infoHash)
           , "peer_id" .= decodeLatin1 (unPeerId peerId)
           , "offer_id" .= offerId o
           , "offer" .= offer o
           ]

data PeerAnswer = PeerAnswer { ansInfoHash :: InfoHash
                             , ansPeerId :: PeerId
                             , ansOfferId :: Text
                             , ansToPeerId :: PeerId
                             , ansAnswer :: Object
                             }

instance ToJSON PeerAnswer where
  toJSON pa =
    object [ "action" .= ("announce" :: Text)
           , "info_hash" .= decodeLatin1 (unInfoHash $ ansInfoHash pa)
           , "peer_id" .= decodeLatin1 (unPeerId $ ansPeerId pa)
           , "offer_id" .= ansOfferId pa
           , "to_peer_id" .= decodeLatin1 (unPeerId $ ansToPeerId pa)
           , "answer" .= ansAnswer pa
           ]

messageToAnswer :: Message -> Maybe PeerAnswer
messageToAnswer msg@(AnnounceMessage {}) =
  PeerAnswer
  <$> Model.InfoHash <$> encodeLatin1 <$> msgInfoHash msg
  <*> (PeerId <$> encodeLatin1 <$> msgPeerId msg)
  <*> msgOfferId msg
  <*> (PeerId <$> encodeLatin1 <$> msgToPeerId msg)
  <*> msgAnswer msg

send :: ToJSON a => a -> WebSocketsT Handler ()
-- send = sendTextData . encode
send a = do
  let m = encode a
  liftIO $ putStrLn $ "WebSocket send: " ++ show m
  sendTextData m

-- | Remember a set of info_hash+peer_id combinations for clean-up on
-- disconnect
type Session = IORef (Set (InfoHash, PeerId))

runWebsocket :: WebSocketsT Handler ()
runWebsocket = do
  liftIO $ putStrLn $ "WebSocket connected."
  session <- liftIO $ newIORef Set.empty
  chan <- liftIO newTChanIO
  r <- tryAny $
    race (recvLoop session chan) (forwardFromHub chan)
  sessionClear session
  case r of
    Left e ->
      liftIO $ putStrLn $ "WebSocket handler error: " ++ show e
    _ ->
      return ()

  sessionClear session
  return ()

forwardFromHub :: TChan Value -> WebSocketsT Handler ()
forwardFromHub chan = loop 0
  where
    keepAliveInterval :: Int
    keepAliveInterval = 30
    loop idleSeconds
      | idleSeconds >= keepAliveInterval = do
          sendPing ("" :: BC.ByteString)
          loop 0
      | otherwise = do
          m <- liftIO $ atomically $
            tryReadTChan chan
          case m of
            Just msg -> do
              send msg
              loop 0
            Nothing -> do
              liftIO $ threadDelay 1000000
              loop $ idleSeconds + 1

recvLoop :: Session -> TChan Value -> WebSocketsT Handler ()
recvLoop session chan = do
  m <- receiveData
  let mJson :: Maybe Message
      mJson = decode m
      mMsgAnn = do
        msg <- mJson
        (msg, ) <$>
          messageToAnnounce msg chan
      mMsgAns =
        mJson >>= messageToAnswer
  case (mMsgAnn, mMsgAns) of
    (Just (msg, ann), _) -> do
      liftIO $ putStrLn "WebSocket tracker request"
      let infoHash = aInfoHash ann
      exists <- lift $ checkExists infoHash
      case exists of
        False ->
          send $ TrackerError "Torrent does not exist. Please go away!"

        True -> do
          liftIO $
            modifyIORef session $
            Set.insert (infoHash, aPeerId ann)

          let isSeeder = aLeft ann == 0

          -- Announce
          tracked <- lift $ trackerTracked <$> getYesod

          ad <- liftIO $ announce tracked ann
          let peers = adPeers ad
          scrape <- liftIO $ scrapeWebtorrent tracked infoHash

          -- Send response
          interval <- liftIO $ randomRIO (1620, 1800)
          send $ TrackerResponse infoHash scrape interval

          -- Distribute offers
          let offers =
                fromMaybe [] $ msgOffers msg
          liftIO $ putStrLn $ "Distribute " ++ show (length offers) ++ " offers to " ++ show (length peers) ++ " peers"
          liftIO $
            forM_ (zip peers offers) $
            \((peerId, peer), offer) ->
              case pConnInfo peer of
                WebtorrentInfo chan ->
                  let peerOffer =
                        toJSON $
                        PeerOffer
                        infoHash (aPeerId ann)
                        offer

                  in atomically $
                     writeTChan chan peerOffer
                _ ->
                  return ()

          -- Time critical stuff done, track stats
          lift $ withDB $ \db -> do
            when (adCompleted ad) $
              addCounter "complete_w" infoHash 1 db
            addCounter "up_w" infoHash (fromIntegral $ adUploaded ad) db
            addCounter "down_w" infoHash (fromIntegral $ adDownloaded ad) db
            setGauge "seeders_w" infoHash (fromIntegral $ scrapeSeeders scrape) db
            setGauge "leechers_w" infoHash (fromIntegral $ scrapeLeechers scrape) db

      -- Loop
      recvLoop session chan
    (_, Just ans) -> do
      tracked <- lift $ trackerTracked <$> getYesod
      mToPeer <- liftIO $
        getPeer tracked (ansInfoHash ans) (ansToPeerId ans)
      case pConnInfo <$> mToPeer of
        Just (WebtorrentInfo chan) ->
          let peerAnswer =
                toJSON ans
          in liftIO $
             atomically $
             writeTChan chan peerAnswer
        _ -> do
          liftIO $
            putStrLn $
            "Cannot relay Websocket answer to " ++ show (ansToPeerId ans)
          return ()
    _ ->
      liftIO $ putStrLn $ "WebSocket received invalid: " ++ show m

sessionClear :: Session -> WebSocketsT Handler ()
sessionClear session = do
  tracked <- lift $ trackerTracked <$> getYesod
  session' <- liftIO $ readIORef session
  forM_ (Set.toList session') $ \(infoHash, peerId) -> do
    liftIO $ clearPeer tracked infoHash peerId

    scrape <- liftIO $ scrapeWebtorrent tracked infoHash
    lift $ withDB $ \db -> do
      setGauge "seeders_w" infoHash (fromIntegral $ scrapeSeeders scrape) db
      setGauge "leechers_w" infoHash (fromIntegral $ scrapeLeechers scrape) db
