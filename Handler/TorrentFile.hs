module Handler.TorrentFile where

import Yesod
import Import
import qualified Model as Model

import Debug.Trace
import qualified Data.ByteString.Char8 as BC


typeTorrent :: ContentType
typeTorrent = "application/x-bittorrent"

newtype RepTorrent = RepTorrent Content

instance HasReps RepTorrent where
  chooseRep (RepTorrent content) _ = return (typeTorrent, content)

getTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFileR user slug (TorrentName name) = do
  torrents <- withDB $
              Model.torrentByName user slug name
  case torrents of
    [] ->
      notFound
    (torrent:_) -> do
      let content = toContent $ torrentTorrent torrent
      trace ("content: " ++ show (BC.length $ torrentTorrent torrent)) $ return $ RepTorrent content
      