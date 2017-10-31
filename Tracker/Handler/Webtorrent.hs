module Tracker.Handler.Webtorrent (getWebTorrentAnnounce) where

import Prelude
import Control.Exception.Enclosed
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Yesod.WebSockets
import Data.Aeson

import Import hiding (Handler)
import Tracker.Foundation
import Tracker.Utils
import Model.Tracker
import qualified Model as Model
import qualified WorkQueue as WQ


getWebTorrentAnnounce :: Handler ()
getWebTorrentAnnounce = do
  addr <- getRemoteAddr
  sessionId <- liftIO $ randomRIO (1, 65535)
  webSockets $ runHandler (addr, sessionId)
  invalidArgs ["WebSocket expected"]


data Message = AnnounceMessage {
  msgInfoHash :: Maybe Text,
  msgPeerId :: Maybe Text,
  msgDownloaded :: Maybe Integer,
  msgUploaded :: Maybe Integer,
  msgLeft :: Maybe Integer,
  msgAction :: Maybe Text,
  msgNumWant :: Maybe Int,
  msgOffers :: Maybe [Value]
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

messageToTrackerRequest :: (PeerAddress, Int) -> Message -> Maybe TrackerRequest
messageToTrackerRequest (addr, port) msg@(AnnounceMessage {}) =
  TrackerRequest
  <$> Model.InfoHash <$> encodeLatin1 <$> msgInfoHash msg
  <*> (PeerId <$> encodeLatin1 <$> msgPeerId msg)
  <*> pure addr
  <*> pure port
  <*> msgUploaded msg
  <*> msgDownloaded msg
  <*> msgLeft msg
  <*> pure (msgAction msg)
  <*> pure False
  <*> pure (encode <$> msgOffers msg)
messageToTrackerRequest _ _ =
  Nothing

encodeLatin1 :: Text -> BC.ByteString
encodeLatin1 = BC.pack . T.unpack

decodeLatin1 :: BC.ByteString -> Text
decodeLatin1 = T.pack . BC.unpack

data TrackerError = TrackerError Text

instance ToJSON TrackerError where
  toJSON (TrackerError reason) =
    object [ "action" .= ("announce" :: Text)
           , "failure reason" .= reason
           , "interval" .= (0xffff :: Int)
           ]

data TrackerResponse = TrackerResponse InfoHash ScrapeInfo Int

instance ToJSON TrackerResponse where
  toJSON (TrackerResponse infoHash scrape interval) =
    object [ "action" .= ("announce" :: Text)
           , "info_hash" .= decodeLatin1 (unInfoHash infoHash)
           , "interval" .= interval
           , "complete" .= scrapeSeeders scrape
           , "incomplete" .= scrapeLeechers scrape
           , "downloaded" .= scrapeDownloaded scrape
           ]

send :: ToJSON a => a -> WebSocketsT Handler ()
send = sendTextData . encode

runHandler :: (PeerAddress, Int) -> WebSocketsT Handler ()
runHandler addr = do
  liftIO $ putStrLn $ "WebSocket connected from " ++ show addr
  r <- tryAny $ handler addr
  case r of
    Left e ->
      liftIO $ putStrLn $ "WebSocket handler error: " ++ show e
    _ ->
      return ()

  -- TODO: clean up
  return ()

handler :: (PeerAddress, Int) -> WebSocketsT Handler ()
handler addr = do
  m <- receiveData
  let mJson :: Maybe Message
      mJson = decode m
  case mJson >>= messageToTrackerRequest addr of
    Just tr -> do
      liftIO $ putStrLn $ "WebSocket tracker request: " ++ show tr
      exists <- lift $ checkExists tr
      case exists of
        False ->
          send $ TrackerError "Torrent does not exist. Please go away!"

        True -> do
          let isSeeder = trLeft tr == 0

          -- Read first
          (peers, scraped) <- lift $ withDB $ \db -> do
            peers <- getPeers (trInfoHash tr) isSeeder db
            scraped <- safeScrape (trInfoHash tr) db
            return (peers, scraped)

          interval <- liftIO $ randomRIO (1620, 1800)
          send $ TrackerResponse (trInfoHash tr) scraped interval

          -- Write in background
          aQ <- trackerAnnounceQueue <$> lift getYesod
          sQ <- trackerScrapeQueue <$> lift getYesod
          pool <- lift getDBPool
          liftIO $ WQ.enqueue aQ $
            do withDBPool pool $ announcePeer tr
               liftIO $ WQ.enqueue sQ $
                 withDBPool pool $ updateScraped $ trInfoHash tr

          -- -- Assemble response
          -- let (peers4, peers6)
          --       | trCompact tr =
          --           ( Benc.BString $ LBC.fromChunks $ concat
          --             [[addr', portToByteString port']
          --                     | TrackedPeer _ (Peer4 addr') port' <- peers]
          --                  , Benc.BString $ LBC.fromChunks $ concat
          --                    [[addr', portToByteString port']
          --                     | TrackedPeer _ (Peer6 addr') port' <- peers]
          --                  )
          --              | otherwise =
          --                  let g peerId addr' port' =
          --                          Benc.BDict [("peer id", Benc.BString $ LBC.fromChunks [peerId]),
          --                                      ("ip", Benc.BString $ LBC.fromChunks [addr']),
          --                                      ("port", Benc.BInt $ fromIntegral port')]
          --                  in ( Benc.BList $
          --                       [g peerId addr' port'
          --                        | TrackedPeer (PeerId peerId) (Peer4 addr') port' <- peers]
          --                     , Benc.BList $
          --                       [g peerId addr' port'
          --                        | TrackedPeer (PeerId peerId) (Peer6 addr') port' <- peers]
          --                     )
          -- return $ RepBenc $
          --   Benc.BDict [ ("peers", peers4)
          --              , ("peers6", peers6)
          --              , ("interval", Benc.BInt interval)
          --              , ("complete", Benc.BInt $ scrapeSeeders scraped + 1)
          --              , ("incomplete", Benc.BInt $ scrapeLeechers scraped)
          --              , ("downloaded", Benc.BInt $ scrapeDownloaded scraped)
          --              ]

          -- Loop
      handler addr
    Nothing ->
      liftIO $ putStrLn $ "WebSocket received invalid from " ++ show addr ++ ": " ++ show m
