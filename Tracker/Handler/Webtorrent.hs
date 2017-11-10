module Tracker.Handler.Webtorrent (getWebTorrentAnnounce) where

import Prelude
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Exception.Enclosed
import System.Random (randomIO, randomRIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import Yesod.WebSockets
import Data.Aeson

import Import hiding (Handler)
import Tracker.Foundation
import Tracker.Utils
import Tracker.WebsocketHub
import Model.Tracker
import qualified Model as Model
import qualified WorkQueue as WQ


getWebTorrentAnnounce :: Handler ()
getWebTorrentAnnounce = do
  addr <- getRemoteAddr
  webSockets $ runHandler addr
  invalidArgs ["WebSocket expected"]

data Offer = Offer { offerId :: Text
                   , offer :: Object
                   } deriving (Show)

instance FromJSON Offer where
  parseJSON (Object o) =
    Offer
    <$> o .: "offer_id"
    <*> o .: "offer"

instance ToJSON Offer where
  toJSON o =
    object [ "offer_id" .= offerId o
           , "offer" .= offer o
           ]

data Message = AnnounceMessage {
  msgInfoHash :: Maybe Text,
  msgPeerId :: Maybe Text,
  msgDownloaded :: Maybe Integer,
  msgUploaded :: Maybe Integer,
  msgLeft :: Maybe Integer,
  msgAction :: Maybe Text,
  msgNumWant :: Maybe Int,
  msgOffers :: Maybe [Offer]
} deriving (Show)

instance FromJSON Message where
  parseJSON (Object o) = do
    action <- o .: "action"
    case action :: Text of
      "announce" ->
        AnnounceMessage
        <$> o .: "info_hash"
        <*> o .: "peer_id"
        <*> o .: "downloaded"
        <*> o .: "uploaded"
        <*> o .: "left"
        <*> o .: "action"
        <*> o .: "numwant"
        <*> o .: "offers"
      _ ->
        fail $ "No supported action: " ++ show action
  parseJSON _ = fail "Not an object"

messageToTrackerRequest :: PeerAddress -> SessionId -> Message -> Maybe TrackerRequest
messageToTrackerRequest addr sessionId msg@(AnnounceMessage {}) =
  TrackerRequest
  <$> Model.InfoHash <$> encodeLatin1 <$> msgInfoHash msg
  <*> (PeerId <$> encodeLatin1 <$> msgPeerId msg)
  <*> pure addr
  <*> pure (fromIntegral sessionId)
  <*> msgUploaded msg
  <*> msgDownloaded msg
  <*> msgLeft msg
  <*> pure (msgAction msg)
  <*> pure False
  <*> pure (encode <$> msgOffers msg)
-- messageToTrackerRequest _ _ =
--   Nothing

data TrackerError = TrackerError Text

instance ToJSON TrackerError where
  toJSON (TrackerError reason) =
    object [ "action" .= ("announce" :: Text)
           , "failure reason" .= reason
           , "interval" .= (0xffff :: Int)
           ]

data TrackerResponse = TrackerResponse InfoHash ScrapeInfo Int

instance ToJSON TrackerResponse where
  toJSON (TrackerResponse infoHash scraped interval) =
    object [ "action" .= ("announce" :: Text)
           , "info_hash" .= decodeLatin1 (unInfoHash infoHash)
           , "interval" .= interval
           , "complete" .= scrapeSeeders scraped
           , "incomplete" .= scrapeLeechers scraped
           , "downloaded" .= scrapeDownloaded scraped
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

send :: ToJSON a => a -> WebSocketsT Handler ()
-- send = sendTextData . encode
send a = do
  let m = encode a
  liftIO $ putStrLn $ "WebSocket send: " ++ show m
  sendTextData m

runHandler :: PeerAddress -> WebSocketsT Handler ()
runHandler addr = do
  liftIO $ putStrLn $ "WebSocket connected from " ++ show addr
  hub <- trackerHub <$> lift getYesod
  session <- liftIO $ do
    sessionId <- randomIO
    newSession hub sessionId
  r <- tryAny $
    race (handler session addr) (forwardFromHub session)
  liftIO $ sessionClear session
  case r of
    Left e ->
      liftIO $ putStrLn $ "WebSocket handler error: " ++ show e
    _ ->
      return ()

  liftIO $ sessionClear session
  return ()

forwardFromHub :: Websession -> WebSocketsT Handler ()
forwardFromHub session = loop 0
  where
    keepAliveInterval :: Int
    keepAliveInterval = 30
    loop idleSeconds
      | idleSeconds >= keepAliveInterval = do
          sendPing ("" :: BC.ByteString)
          loop 0
      | otherwise = do
          m <- liftIO $ sessionRecv session
          case m of
            Just msg -> do
              send msg
              loop 0
            Nothing -> do
              liftIO $ threadDelay 1000000
              loop $ idleSeconds + 1

handler :: Websession -> PeerAddress -> WebSocketsT Handler ()
handler session addr = do
  m <- receiveData
  let mJson :: Maybe Message
      mJson = decode m
      mTr = do
        msg <- mJson
        (msg, ) <$>
          messageToTrackerRequest addr (sessionId session) msg
  case mTr of
    Just (msg, tr) -> do
      liftIO $ putStrLn $ "WebSocket tracker request: " ++ show tr
      exists <- lift $ checkExists tr
      case exists of
        False ->
          send $ TrackerError "Torrent does not exist. Please go away!"

        True -> do
          let isSeeder = trLeft tr == 0
              infoHash = trInfoHash tr

          -- Read first
          (peers, scraped) <- lift $ withDB $ \db -> do
            peers <- filter (\(TrackedWebPeer peerId _) ->
                               peerId /= trPeerId tr
                            ) <$>
                     getWebPeers infoHash isSeeder db
            scraped <- safeScrape infoHash db
            return (peers, scraped)

          -- Write in background
          aQ <- trackerAnnounceQueue <$> lift getYesod
          sQ <- trackerScrapeQueue <$> lift getYesod
          pool <- lift getDBPool
          liftIO $ WQ.enqueue aQ $
            do withDBPool pool $ announcePeer tr
               liftIO $ putStrLn $ "Peer announced"
               liftIO $ WQ.enqueue sQ $
                 withDBPool pool $ updateScraped infoHash
               liftIO $ putStrLn $ "Update scraped done"

          -- Send response
          interval <- liftIO $ randomRIO (1620, 1800)
          send $ TrackerResponse infoHash scraped interval

          -- Distribute offers
          let offers =
                fromMaybe [] $ msgOffers msg
          liftIO $ putStrLn $ "Distribute " ++ show (length offers) ++ " offers to " ++ show (length peers) ++ " peers"
          lift $
            withDB $ \db ->
            forM_ (zip peers offers) $
            \((TrackedWebPeer peerId _offers), offer) ->
            hubSend db (PeerOffer infoHash peerId offer)

      -- Loop
      handler session addr
    Nothing ->
      liftIO $ putStrLn $ "WebSocket received invalid from " ++ show addr ++ ": " ++ show m
