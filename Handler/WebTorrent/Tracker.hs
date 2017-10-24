module Handler.WebTorrent.Tracker (getWebTorrentAnnounce) where

import Yesod.WebSockets
import Data.Aeson

import Import


getWebTorrentAnnounce :: Handler ()
getWebTorrentAnnounce = do
  webSockets handler
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
    

handler = do
  m <- receiveData
  let mJson :: Maybe Message
      mJson = decode m
  liftIO $ putStrLn $ "WebSocket received: " ++ show mJson
  handler
