module Handler.TorrentFile where

import Prelude (head)
import Data.Convertible (convert)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Yesod
import Yesod.Default.Config (appExtra)
import Blaze.ByteString.Builder

import Import
import qualified Model as Model
import PathPieces (HexInfoHash(HexInfoHash))
import Benc


typeTorrent :: ContentType
typeTorrent = "application/x-bittorrent"

newtype RepTorrent = RepTorrent Content
    deriving (ToContent)

instance ToTypedContent RepTorrent where
    toTypedContent =
        TypedContent "application/x-bittorrent" .
        toContent

-- |Does the same but is on different routes for stats.
getTorrentFileForWebtorrentR :: UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFileForWebtorrentR = getTorrentFile True

getTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFileR = getTorrentFile False

getTorrentFile :: Bool -> UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFile includeLocalWebseeder user slug (TorrentName name) = do
  mInfo <- withDB $ \db -> do
    torrents <- Model.torrentByName user slug name db
    case torrents of
      [] ->
        return Nothing
      (torrent:_) -> do
        let buf = torrentTorrent torrent
            infoHash = torrentInfoHash torrent
        return $ Just (buf, infoHash)
  case mInfo of
    Nothing ->
      notFound
    Just (buf, infoHash) -> do
      seedUrl <- ($ WebSeedR (HexInfoHash infoHash) name) <$>
                 getFullUrlRender
      let seedUrls
            | includeLocalWebseeder = [seedUrl]
            | otherwise = []
      myTrackers <- map convert .
                    extraTrackerURLs .
                    appExtra . settings <$>
                    getYesod
      let mBuf = updateTorrent myTrackers seedUrls buf
      case mBuf of
        Just buf' ->
          return $ RepTorrent $ toContent buf'
        Nothing -> do
          liftIO $
            putStrLn $ "Cannot parse preexisting torrent " ++
            show user ++ "/" ++
            show slug ++ "/" ++
            show name
          return $ RepTorrent $ toContent buf

-- |Updates the following keys:
-- * `announce` (BEP 3)
-- * `announce-list` (BEP 12)
-- * `url-list` (BEP 19)
updateTorrent :: [LBC.ByteString] -> [Text] -> BC.ByteString -> Maybe Builder
updateTorrent myTrackers seedUrls buf = do
  BDict dict <- parseBenc buf
  let
    getList :: LBC.ByteString -> [BValue]
    getList name =
      foldl (\result pair ->
               case pair of
                 (BString name', BList trackerList)
                   | name == name' ->
                     trackerList
                 _ ->
                   result
            ) [] dict
    trackerList = do
      BList trackers <- getList "announce-list"
      return $ do
        BString tracker <- trackers
        return tracker
    trackerList' =
      map (: []) myTrackers ++
      (filter (not . null) $
       map (filter (not . (`elem` myTrackers)))
       trackerList)
    urlList = do
      BString url <- getList "url-list"
      return url
    urlList' =
      map (convert . encodeUtf8) seedUrls ++
      urlList
    dict' =
        ("announce", BString $ head myTrackers) :
        ("announce-list", BList $ map (BList . map BString) trackerList') :
        ("url-list", BList $ map BString urlList') :
        filter (\(name, _val) ->
                   name /= "announce" &&
                   name /= "announce-list" &&
                   name /= "url-list"
               ) dict

  return $ toBuilder $
    BDict dict'
