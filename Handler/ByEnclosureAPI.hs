{-# LANGUAGE TupleSections #-}
module Handler.ByEnclosureAPI where

import Prelude
import Yesod
import qualified Data.Text as T
import Control.Monad

import Import
import Model.Download

-- TODO: Access-Control-Allow-Origin: *
getByEnclosureJson :: Handler RepJson
getByEnclosureJson = do
  urls <- map snd `fmap`
          filter (\(k, v) ->
                   "url" `T.isPrefixOf` k
                 ) `fmap`
          reqGetParams `fmap` 
          getRequest
  urlDownloads <- withDB $ \db ->
    forM urls $ \url ->
        (url, ) `fmap`
        enclosureDownloads url db
  -- Drop enclosures with no associated download
  let urlDownloads' = filter ((> 0) . length . snd) urlDownloads
  RepJson `fmap` 
          toContent `fmap`
          object `fmap` 
          mapM (\(url, downloads) -> do
                  json <- downloadToJson downloads
                  return $ url .= json
               ) urlDownloads'

    where downloadToJson downloads@(d:_) = do
                       sources <- mapM sourceToJson downloads
                       return $
                        object [ "info_hash" .= infoHashToHex (downloadInfoHash d)
                               , "size" .= downloadSize d
                               , "seeders" .= downloadSeeders d
                               , "leechers" .= downloadLeechers d
                               , "upspeed" .= downloadUpspeed d
                               , "downspeed" .= downloadDownspeed d
                               , "downloaded" .= downloadDownloaded d
                               , "sources" .= sources
                               ]
          sourceToJson d = do
                       do torrentLink' <- torrentLink d
                          permaLink' <- permaLink d
                          return $
                           object [ "torrent" .= torrentLink'
                                  , "permalink" .= permaLink'
                                  , "item.id" .= downloadItem d
                                  , "item.title" .= downloadTitle d
                                  , "item.published" .= iso8601 (downloadPublished d)
                                  , "item.homepage" .= downloadHomepage d
                                  , "item.payment" .= downloadPayment d
                                  , "item.image" .= downloadImage d
                                  , "feed.title" .= downloadFeedTitle d
                                  ]

torrentLink d = 
    ($ TorrentFileR 
           (downloadUser d) 
           (downloadSlug d) 
           (TorrentName $ downloadName d)
    ) `fmap`
    getUrlRender

permaLink d =
    ((`T.append` (downloadItem d)) .
     (`T.append` "#") .
     -- TODO: UserFeedR
     ($ TorrentFileR (downloadUser d) (downloadSlug d) (TorrentName "TODO"))) `fmap`
    getUrlRender
    