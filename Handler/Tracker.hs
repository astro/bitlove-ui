{-# LANGUAGE TupleSections #-}
module Handler.Tracker where

import Prelude
import Yesod
import qualified Network.Wai as Wai
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.Socket (SockAddr (..), HostName, getAddrInfo, addrAddress, tupleToHostAddress, tupleToHostAddress6)
import Data.Word (Word32)
import Data.Bits
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)

import Foundation (DBPool, HasDB (getDBPool), withDB, withDBPool, Transaction)
import qualified Model as Model
import Model.Tracker
import qualified Benc as Benc
import qualified WorkQueue as WQ
import Cache


-- TODO: make configurable
ourPeerId :: PeerId
ourPeerId = PeerId "-<30000-bitlove.org/"

ourSeeders :: [TrackedPeer]
ourSeeders = do
  addr <-  [ Peer4 "\85\10\246\236"
           , Peer6 "\x2a\x01\x04\xf8\x01\x60\x54\x21\x00\x00\x00\x00\x00\x00\x00\x03"
           ]
  return $ TrackedPeer ourPeerId addr 6881

data TrackerApp = TrackerApp
    { trackerDBPool :: DBPool
    , trackerCache :: Cache
    , trackerAnnounceQueue :: WQ.Queue
    , trackerScrapeQueue :: WQ.Queue
    }

mkYesod "TrackerApp" [parseRoutes|
                      /announce AnnounceR GET
                      /scrape ScrapeR GET
                      |]

instance Yesod TrackerApp where
    makeSessionBackend _ = return Nothing

instance HasDB TrackerApp where
    getDBPool = trackerDBPool <$> getYesod

makeTrackerApp :: DBPool -> IO TrackerApp
makeTrackerApp pool =
    do cache <- newCache "localhost" 11211
       aq <- WQ.makeQueue
       sq <- WQ.makeQueue
       return $ TrackerApp pool cache aq sq

getCache :: Handler Cache
getCache = trackerCache <$> getYesod

newtype RepBenc = RepBenc Benc.BValue

instance ToContent RepBenc where
    toContent (RepBenc v) =
        ContentBuilder (Benc.toBuilder v) Nothing

instance ToTypedContent RepBenc where
    toTypedContent =
        TypedContent "application/x-bittorrent" .
        toContent


-- TODO: support key parameter
getAnnounceR :: Handler RepBenc
getAnnounceR = do
  query <- getRawQuery
  addr <- getRemoteAddr
  let q :: BC.ByteString -> Maybe BC.ByteString
      q = join . (`lookup` query)
      qi :: Read r => BC.ByteString -> Maybe r
      qi name = do v <- q name
                   case readsPrec 0 $ BC.unpack v of
                     [(r, "")] -> return r
                     _ -> Nothing
      mTr = TrackerRequest <$>
            Model.InfoHash <$> q "info_hash" <*>
            (PeerId <$> q "peer_id") <*>
            pure addr <*>
            qi "port" <*>
            qi "uploaded" <*>
            qi "downloaded" <*>
            pure (fromMaybe 1 $ qi "left") <*>
            pure (decodeUtf8 <$> q "event") <*>
            pure (maybe False (const True) $ q "compact")

      checkExists tr = do
        let infoHash = trInfoHash tr
        cachedExists <- getCache >>=
                        liftIO . getTorrentExists infoHash
        case cachedExists of
          False ->
            return False
          True -> do
            torrentExists <- withDB $ Model.infoHashExists infoHash
            case torrentExists of
              True -> return True
              False -> do
                getCache >>=
                  liftIO . setTorrentExists infoHash False
                return False

  case mTr of
    Nothing ->
        return $ RepBenc $
        Benc.BDict [(Benc.BString "failure reason",
                     Benc.BString "Invalid tracker request"),
                    (Benc.BString "interval",
                     Benc.BInt 0xffff)]
    Just tr -> do
      exists <- checkExists tr
      case exists of
        False ->
            return $ RepBenc $
            Benc.BDict [(Benc.BString "failure reason",
                         Benc.BString "Torrent does not exist. Please go away!"),
                        (Benc.BString "interval",
                         Benc.BInt 0xffff)]
        True ->
            do let isSeeder = trLeft tr == 0
               -- Read first
               (peers, scraped) <-
                   withDB $ \db ->
                       do peers <- (ourSeeders ++) <$> getPeers (trInfoHash tr) isSeeder db
                          scraped <- safeScrape (trInfoHash tr) db
                          return (peers, scraped)
               -- Write in background
               aQ <- trackerAnnounceQueue <$> getYesod
               sQ <- trackerScrapeQueue <$> getYesod
               pool <- getDBPool
               liftIO $ WQ.enqueue aQ $
                      do withDBPool pool $ announcePeer tr
                         liftIO $ WQ.enqueue sQ $
                           withDBPool pool $ updateScraped $ trInfoHash tr
               -- Assemble response
               let (peers4, peers6)
                       | trCompact tr =
                           ( Benc.BString $ LBC.fromChunks $ concat
                             [[addr', portToByteString port']
                              | TrackedPeer _ (Peer4 addr') port' <- peers]
                           , Benc.BString $ LBC.fromChunks $ concat
                             [[addr', portToByteString port']
                              | TrackedPeer _ (Peer6 addr') port' <- peers]
                           )
                       | otherwise =
                           let g peerId addr' port' =
                                   Benc.BDict [("peer id", Benc.BString $ LBC.fromChunks [peerId]),
                                               ("ip", Benc.BString $ LBC.fromChunks [addr']),
                                               ("port", Benc.BInt $ fromIntegral port')]
                           in ( Benc.BList $
                                [g peerId addr' port'
                                 | TrackedPeer (PeerId peerId) (Peer4 addr') port' <- peers]
                              , Benc.BList $
                                [g peerId addr' port'
                                 | TrackedPeer (PeerId peerId) (Peer6 addr') port' <- peers]
                              )
               interval <- liftIO $ randomRIO (1620, 1800)
               return $ RepBenc $
                      Benc.BDict [ ("peers", peers4)
                                 , ("peers6", peers6)
                                 , ("interval", Benc.BInt interval)
                                 , ("complete", Benc.BInt $ scrapeSeeders scraped + 1)
                                 , ("incomplete", Benc.BInt $ scrapeLeechers scraped)
                                 , ("downloaded", Benc.BInt $ scrapeDownloaded scraped)
                                 ]

getScrapeR :: Handler RepBenc
getScrapeR = do
  query <- getRawQuery
  let mInfoHash = Model.InfoHash <$>
                  (join $ "info_hash" `lookup` query)
  (infoHash, scraped) <-
      case mInfoHash of
        Nothing ->
            notFound
        Just infoHash ->
            (infoHash, ) <$>
            withDB (safeScrape infoHash)

  return $ RepBenc $
         Benc.BDict
         [("host",
           Benc.BDict
           [(Benc.BString $ LBC.fromChunks [Model.unInfoHash infoHash],
             Benc.BDict
             [("incomplete", Benc.BInt $ scrapeLeechers scraped),
              ("complete", Benc.BInt $ scrapeSeeders scraped + 1),
              ("downloaded", Benc.BInt $ scrapeDownloaded scraped)]
            )]
          )]

safeScrape :: Model.InfoHash -> Transaction ScrapeInfo
safeScrape infoHash db =
    (head . (++ [ScrapeInfo 0 0 0 0])) <$>
    scrape infoHash db

getRawQuery :: Handler [(BC.ByteString, Maybe BC.ByteString)]
getRawQuery =
    Wai.queryString <$>
    waiRequest

getRemoteAddr :: Handler PeerAddress
getRemoteAddr = do
  req <- waiRequest
  let remote = normalize $ Wai.remoteHost req
  toPeerAddress <$>
    case isLocalhost remote of
      False -> return remote
      True ->
        liftIO $
        fromMaybe (return remote) $
        fmap (parseAddr . BC.unpack) $
        "X-Real-IP" `lookup` Wai.requestHeaders req

      where
        normalize (SockAddrInet6 port _ (0, 0, 0xffff, haddr) _) =
          SockAddrInet port haddr
        normalize sockaddr =
          sockaddr

        toPeerAddress (SockAddrInet _ haddr) =
          Peer4 $ wordToByteString haddr
        toPeerAddress (SockAddrInet6 _ _ (h6, h6', h6'', h6''') _) =
          Peer6 $ B.concat [ wordToByteString h6
                           , wordToByteString h6'
                           , wordToByteString h6''
                           , wordToByteString h6'''
                           ]
        toPeerAddress _ =
          error "Must not happen"

        isLocalhost (SockAddrInet _ addr)
          | addr == tupleToHostAddress (127, 0, 0, 1) =
              True
        isLocalhost (SockAddrInet6 _ _ addr _)
          | addr == tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 1) =
              True
        isLocalhost _ =
          False

        parseAddr :: HostName -> IO SockAddr
        parseAddr s =
          addrAddress <$>
          head <$>
          getAddrInfo Nothing (Just s) Nothing

wordToByteString :: Word32 -> B.ByteString
wordToByteString w =
    B.pack $
    map (fromIntegral . (.&. 0xFF) . (w `shiftR`) . (* 8)) $
    reverse [0..3]

portToByteString :: Int -> B.ByteString
portToByteString p =
  B.pack $
  map (fromIntegral . (.&. 0xFF)) $
  [ p `shiftR` 8
  , p
  ]
