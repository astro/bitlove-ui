module Handler.TorrentFile where

import Yesod
import Import
import qualified Model as Model


typeTorrent :: ContentType
typeTorrent = "application/x-bittorrent"

newtype RepTorrent = RepTorrent Content
    deriving (ToContent)

instance ToTypedContent RepTorrent where
    toTypedContent =
        TypedContent "application/x-bittorrent" .
        toContent

getTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFileR user slug (TorrentName name) = do
  torrents <- withDB $
              Model.torrentByName user slug name
  case torrents of
    [] ->
      notFound
    (torrent:_) -> do
      let content = toContent $ torrentTorrent torrent
      return $ RepTorrent content
      