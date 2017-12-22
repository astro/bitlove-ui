module Handler.TorrentFile where

import Prelude (head)
import Data.Convertible (convert)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Yesod
import Blaze.ByteString.Builder
import Network.URI (escapeURIString, isUnescapedInURIComponent)

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
getTorrentFileForWebtorrentR = getTorrentFileR

getTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepTorrent
getTorrentFileR user slug (TorrentName name) = do
  mInfo <- withDB $ \db -> do
    torrents <- Model.torrentByName user slug name db
    case torrents of
      [] ->
        return Nothing
      (torrent:_) -> do
        let buf = torrentTorrent torrent
            infoHash = torrentInfoHash torrent
        guids <- Model.torrentGuids infoHash db
        return $ Just (buf, infoHash, guids)
  case mInfo of
    Nothing ->
      notFound
    Just (buf, infoHash, guids) -> do
      seedUrl <- T.append "https://bitlove.org" <$>
                 ($ WebSeedR (HexInfoHash infoHash) name) <$>
                 getUrlRender
      let mBuf = updateTorrent [seedUrl] guids buf
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
updateTorrent :: [Text] -> [Text] -> BC.ByteString -> Maybe Builder
updateTorrent seedUrls guids buf = do
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
      myUrls ++
      urlList
    dict' =
        ("announce", BString $ head myTrackers) :
        ("announce-list", BList $ map (BList . map BString) trackerList') :
        ("url-list", BList $ map BString urlList') :
        filter (\(name, val) ->
                  name /= "announce" &&
                 name /= "announce-list" &&
                 name /= "url-list"
               ) dict

  return $ toBuilder $
    BDict dict'

    where
      myTrackers =
        [ "http://t.bitlove.org/announce"
        , "wss://t.bitlove.org/webtorrent-announce" ]
      myUrls = do
        guid <- guids
        return $
          LBC.fromChunks
          [encodeUtf8 $
           webseed1FromGuid guid
          ]

webseed1FromGuid :: Text -> Text
webseed1FromGuid =
  ("https://cdn.streamoff.de/content/" `T.append`) .
  T.pack .
  escapeURIString isUnescapedInURIComponent .
  map (\x -> if x == '/' then '_' else x) .
  T.unpack {- .
  head . tail . T.splitOn "#" -}
