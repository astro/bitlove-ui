{-# LANGUAGE TupleSections #-}
module Handler.ByEnclosureAPI where

import Prelude
import Yesod
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Text.Regex.PCRE.ByteString as PCRE
import Control.Monad
import Data.Time
import Data.Maybe (catMaybes)
import System.IO.Unsafe

import Import

getByEnclosureJson :: Handler RepJson
getByEnclosureJson = do
  -- For CORS:
  addHeader "Access-Control-Allow-Origin" "*"

  params <- reqGetParams <$> getRequest
  let fromParams :: T.Text -> [T.Text]
      fromParams prefix = map snd $
                          filter ((prefix `T.isPrefixOf`) . fst)
                          params
      urls = fromParams "url"
      guids = fromParams  "guid"
  resDownloads <- withDB $ \db ->
                  do urlDownloads <-
                         forM urls $ \url ->
                             (url, ) <$>
                             let url' = rewriteUrl url
                             in enclosureDownloads url' db
                     guidDownloads' <-
                         forM guids $ \guid ->
                             (guid, ) <$>
                             guidDownloads guid db
                     return $ urlDownloads ++ guidDownloads'
  -- Drop enclosures with no associated download
  let resDownloads' = filter (not . null . snd) resDownloads
  RepJson .
          toContent .
          object <$>
          mapM (\(url, downloads) -> do
                  json <- downloadToJson downloads
                  return $ url .= json
               ) resDownloads'

    where downloadToJson downloads@(d:_) = do
                       sources <- mapM sourceToJson downloads
                       return $
                        object [ "info_hash" .= infoHashToHex (downloadInfoHash d)
                               , "size" .= downloadSize d
                               , "seeders" .= (downloadSeeders d + 1)
                               , "leechers" .= downloadLeechers d
                               , "upspeed" .= downloadUpspeed d
                               , "downspeed" .= downloadDownspeed d
                               , "downloaded" .= downloadDownloaded d
                               , "sources" .= sources
                               ]
          downloadToJson [] = error "This never occurs"
          sourceToJson d = do
                          torrentLink' <- torrentLink d
                          permaLink' <- permaLink d
                          tz <- liftIO getCurrentTimeZone
                          return $
                           object [ "torrent" .= torrentLink'
                                  , "permalink" .= permaLink'
                                  , "item.id" .= downloadItem d
                                  , "item.title" .= downloadTitle d
                                  , "item.published" .=
                                                     iso8601
                                                     (localTimeToZonedTime tz $
                                                      downloadPublished d)
                                  , "item.homepage" .= downloadHomepage d
                                  , "item.payment" .= downloadPayment d
                                  , "item.image" .= downloadImage d
                                  , "feed.title" .= downloadFeedTitle d
                                  ]
          rewriteUrl :: Text -> Text
          rewriteUrl t =
              let t' = encodeUtf8 t
                  runRe re buf =
                      unsafePerformIO $
                      do Right regex <- PCRE.compile PCRE.compBlank PCRE.execBlank re
                         result <- PCRE.regexec regex buf
                         case result of
                           Left (_, e) -> error e
                           Right Nothing -> return Nothing
                           Right (Just (_, _, _, matches)) -> return $ Just matches

                  rePodpress = "^(.+?\\/podpress_trac\\/)web(\\/\\d+\\/\\d+\\/.+)$"
                  rewritePodpress =
                      do [_, r1, r2] <- runRe rePodpress t'
                         return $
                                T.concat [ decodeUtf8 r1
                                         , "feed"
                                         , decodeUtf8 r2
                                         ]
                  reBlubrry1 = "^(https?:\\/\\/media\\.blubrry\\.com\\/.+?\\/)p\\/(.+?\\/)p\\/(.+?)$"
                  rewriteBlubrry1 =
                      do [_, r1, r2, r3] <- runRe reBlubrry1 t'
                         return $
                                T.concat [ decodeUtf8 r1
                                         , decodeUtf8 r2
                                         , decodeUtf8 r3
                                         ]
                  reBlubrry2 = "^(https?:\\/\\/media\\.blubrry\\.com\\/.+?\\/)p\\/(.+?)$"
                  rewriteBlubrry2 =
                      do [_, r1, r2] <- runRe reBlubrry2 t'
                         return $
                                T.concat [ decodeUtf8 r1
                                         , decodeUtf8 r2
                                         ]
              in head $
                 catMaybes [ rewritePodpress
                           , rewriteBlubrry1
                           , rewriteBlubrry2
                           , Just t
                           ]

torrentLink :: Download -> HandlerT UIApp IO Text
torrentLink d = 
    ($ TorrentFileR 
           (downloadUser d) 
           (downloadSlug d) 
           (TorrentName $ downloadName d)
    ) `fmap`
    getFullUrlRender

permaLink :: Download -> HandlerT UIApp IO Text
permaLink d =
    ((`T.append` downloadItem d) .
     (`T.append` "#") .
     ($ UserFeedR (downloadUser d) (downloadSlug d))) `fmap`
    getFullUrlRender
    