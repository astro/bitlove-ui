{-# LANGUAGE TupleSections #-}
module Tracker.Handler.HTTP where

import Prelude
import Yesod
import qualified Network.Wai as Wai
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)

import Foundation (HasDB (getDBPool), withDB, withDBPool, Transaction)
import qualified Model as Model
import Model.Tracker
import qualified Benc as Benc
import qualified WorkQueue as WQ
import Cache
import Tracker.Foundation
import Tracker.Utils


-- TODO: make configurable
ourPeerId :: PeerId
ourPeerId = PeerId "-<30000-bitlove.org/"

ourSeeders :: [TrackedPeer]
ourSeeders = do
  addr <-  [ Peer4 "\94\130\10\164"
           , Peer6 "\x2a\x01\x04\xf8\x01\x0b\x0e\xdb\x00\x00\x00\x00\x00\x00\x00\x03"
           ]
  return $ TrackedPeer ourPeerId addr 6881


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
            pure (maybe False (const True) $ q "compact") <*>
            pure Nothing

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
