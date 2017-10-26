module Tracker.Handler.Webtorrent (getWebTorrentAnnounce) where

import Yesod.WebSockets
import Data.Aeson

import Import hiding (Handler)
import Tracker.Foundation
import Tracker.Utils
import Model.Tracker


getWebTorrentAnnounce :: Handler ()
getWebTorrentAnnounce = do
  addr <- getRemoteAddr
  webSockets $ handler addr
  invalidArgs ["WebSocket expected"]


data Message = Message {
  msgInfoHash :: Text,
  msgPeerId :: Text,
  msgDownloaded :: Integer,
  msgUploaded :: Integer,
  msgLeft :: Integer,
  msgAction :: Text,
  msgNumWant :: Int,
  msgOffers :: [Value]
} deriving (Show)

instance FromJSON Message where
  parseJSON (Object o) = Message
    <$> o .: "info_hash"
    <*> o .: "peer_id"
    <*> o .: "downloaded"
    <*> o .: "uploaded"
    <*> o .: "left"
    <*> o .: "action"
    <*> o .: "numwant"
    <*> o .: "offers"
    
handler :: PeerAddress -> WebSocketsT Handler ()
handler addr = do
  me <- receiveDataE
  case me of
    Left e ->
      liftIO $ putStrLn $ "WebSocket error: " ++ show e
    Right m -> do
      let mJson :: Maybe Message
          mJson = decode m
      liftIO $ putStrLn $ "WebSocket received from " ++ show addr ++ ": " ++ show mJson
      handler addr
