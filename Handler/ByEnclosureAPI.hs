{-# LANGUAGE TupleSections #-}
module Handler.ByEnclosureAPI where

import Prelude
import Yesod
import qualified Data.Text as T
import Control.Monad

import Import
import Model.Download

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
  urlRender <- getUrlRender
  return $ RepJson $ toContent $
         object $ map (\(url, downloads@(d:_)) ->
                        url .= object [
                                 "info_hash" .= infoHashToHex (downloadInfoHash d),
                                 "size" .= downloadSize d,
                                 "seeders" .= downloadSeeders d,
                                 "leechers" .= downloadLeechers d,
                                 "upspeed" .= downloadUpspeed d,
                                 "downspeed" .= downloadDownspeed d,
                                 "downloaded" .= downloadDownloaded d,
                                 "sources" .= (map (\d' ->
                                                    object [
                                                     "torrent" .= urlRender (TorrentFileR (downloadUser d') (downloadSlug d') $ TorrentName $ downloadName d')
                                                    ]
                                                   ) downloads)
                                ]
                      ) urlDownloads
