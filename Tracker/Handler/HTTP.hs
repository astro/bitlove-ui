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

import Foundation (withDB)
import qualified Model
import qualified Benc as Benc
import Tracked
import Tracker.Foundation
import Tracker.Utils


-- TODO: make configurable
ourPeerId :: PeerId
ourPeerId = PeerId "-<30000-bitlove.org/"

ourSeeders :: [(PeerId, PeerAddress, Int)]
ourSeeders = do
  addr <-  [ Peer4 "\94\130\10\164"
           , Peer6 "\x2a\x01\x04\xf8\x01\x0b\x0e\xdb\x00\x00\x00\x00\x00\x00\x00\x03"
           ]
  return (ourPeerId, addr, 6881)


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
      compact = fromMaybe False $
                ("1" ==) <$>
                q "compact"
      mAnn = TrackedAnnounce <$>
             Model.InfoHash <$> q "info_hash" <*>
             (PeerId <$> q "peer_id") <*>
             (BittorrentInfo addr <$>
              qi "port") <*>
             qi "uploaded" <*>
             qi "downloaded" <*>
             pure (fromMaybe 1 $ qi "left") <*>
             pure (decodeUtf8 <$> q "event")

  case mAnn of
    Nothing ->
        return $ RepBenc $
        Benc.BDict [(Benc.BString "failure reason",
                     Benc.BString "Invalid tracker request"),
                    (Benc.BString "interval",
                     Benc.BInt 0xffff)]
    Just ann -> do
      let infoHash = aInfoHash ann
      exists <- checkExists infoHash
      case exists of
        False ->
            return $ RepBenc $
            Benc.BDict [(Benc.BString "failure reason",
                         Benc.BString "Torrent does not exist. Please go away!"),
                        (Benc.BString "interval",
                         Benc.BInt 0xffff)]
        True ->
            do tracked <- trackerTracked <$> getYesod
               -- Announce!
               ad <- liftIO $ announce tracked ann
               scrape <- liftIO $ scrapeBittorrent tracked infoHash

               -- Track stats
               withDB $ \db -> do
                 when (adCompleted ad) $
                   Model.addCounter "complete_w" infoHash 1 db
                 Model.addCounter "up_w" infoHash (fromIntegral $ adUploaded ad) db
                 Model.addCounter "down_w" infoHash (fromIntegral $ adDownloaded ad) db
                 Model.setGauge "seeders_w" infoHash (fromIntegral $ scrapeSeeders scrape) db
                 Model.setGauge "leechers_w" infoHash (fromIntegral $ scrapeLeechers scrape) db

              -- Assemble response
               let peers =
                     ourSeeders ++
                     map (\(peerId, peer) ->
                             let BittorrentInfo pAddr pPort =
                                   pConnInfo peer
                             in (peerId, pAddr, pPort)
                         ) (adPeers ad)
               let (peers4, peers6)
                       | compact =
                           ( Benc.BString $ LBC.fromChunks $ concat
                             [[addr', portToByteString port']
                              | (_, Peer4 addr', port') <- peers]
                           , Benc.BString $ LBC.fromChunks $ concat
                             [[addr', portToByteString port']
                              | (_, Peer6 addr', port') <- peers]
                           )
                       | otherwise =
                           let g peerId addr' port' =
                                   Benc.BDict [("peer id", Benc.BString $ LBC.fromChunks [peerId]),
                                               ("ip", Benc.BString $ LBC.fromChunks [addr']),
                                               ("port", Benc.BInt $ fromIntegral port')]
                           in ( Benc.BList $
                                [g peerId addr' port'
                                 | (PeerId peerId, Peer4 addr', port') <- peers]
                              , Benc.BList $
                                [g peerId addr' port'
                                 | (PeerId peerId, Peer6 addr', port') <- peers]
                              )
               interval <- liftIO $ randomRIO (1620, 1800)
               -- TODO: stats
               return $ RepBenc $
                 -- TODO: "downloaded"
                      Benc.BDict [ ("peers", peers4)
                                 , ("peers6", peers6)
                                 , ("interval", Benc.BInt interval)
                                 , ("complete", Benc.BInt $ fromIntegral $
                                                scrapeSeeders scrape + 1)
                                 , ("incomplete", Benc.BInt $ fromIntegral $
                                                  scrapeLeechers scrape)
                                 ]

getScrapeR :: Handler RepBenc
getScrapeR = do
  query <- getRawQuery
  let mInfoHash = Model.InfoHash <$>
                  (join $ "info_hash" `lookup` query)
  infoHash <- case mInfoHash of
    Nothing ->
      invalidArgs ["Missing info_hash"]
    Just infoHash -> do
      exists <- checkExists infoHash
      when (not exists)
        notFound
      return infoHash

  tracked <- trackerTracked <$> getYesod
  scrape <- liftIO $ scrapeBittorrent tracked infoHash

  return $ RepBenc $
         Benc.BDict
         [("host",
           Benc.BDict
           [(Benc.BString $ LBC.fromChunks [Model.unInfoHash infoHash],
             Benc.BDict
             -- TODO: "downloaded"
             [("incomplete", Benc.BInt $ fromIntegral $
                             scrapeLeechers scrape),
              ("complete", Benc.BInt $ fromIntegral $
                           scrapeSeeders scrape + 1)]
            )]
          )]

getRawQuery :: Handler [(BC.ByteString, Maybe BC.ByteString)]
getRawQuery =
    Wai.queryString <$>
    waiRequest
