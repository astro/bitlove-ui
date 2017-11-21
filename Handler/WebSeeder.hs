module Handler.WebSeeder where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.String (IsString)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Yesod
import Data.Conduit
import Network.HTTP.Conduit hiding (proxy)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.Binary.Builder (fromByteString)
import qualified Data.CaseInsensitive as CI

import Prelude
import Foundation
import PathPieces
import Model.Download (torrentEnclosures)
import Model.Stats (addCounter)


hAccessControlAllowOrigin :: IsString s => s
hAccessControlAllowOrigin = "Access-Control-Allow-Origin"

corsOrigins :: [Text]
corsOrigins =
  map T.toCaseFold
  ["http://bitlove.org",
    "http://www.bitlove.org",
    "https://bitlove.org",
    "https://www.bitlove.org"
  ]

isOriginAllowed :: Text -> Bool
isOriginAllowed =
  (`elem` corsOrigins) .
  T.toCaseFold


optionsWebSeedR :: HexInfoHash -> Handler () --ContentType, Content)
optionsWebSeedR _ = do
  mMethod <- lookupHeader "Access-Control-Request-Method"
  when (mMethod /= Just "GET") $
    badMethod

  mOrigin <- (decodeUtf8 <$>) <$>
             lookupHeader hOrigin

  case mOrigin of
    Just origin
      | isOriginAllowed origin ->
          addHeader hAccessControlAllowOrigin origin
    Just _ ->
      invalidArgs ["Invalid Origin: header"]
    Nothing ->
      invalidArgs ["Missing Origin: header"]

  addHeader "Access-Control-Allow-Headers" "Range"
  addHeader "Access-Control-Max-Age" "86400"

getWebSeedR :: HexInfoHash -> Handler (ContentType, Content)
getWebSeedR (HexInfoHash infoHash) = do
  urls <- nub <$>
          withDB (torrentEnclosures infoHash)
  liftIO $ putStrLn $
    "Seeding " ++ show infoHash ++
    " with URLs: " ++
    concatMap (\url ->
                 "\n" ++ T.unpack url
              ) urls

  mOrigin <- (decodeUtf8 <$>) <$>
             lookupHeader hOrigin
  mRange <- lookupHeader hRange
  manager <- httpManager <$> getYesod
  let tryUrl :: [Text] -> Handler (ContentType, Content)
      tryUrl (url:urls') = do
        req <- parseRequest $ T.unpack url
        let reqHeaders =
              (hUserAgent, userAgent) :
              case mRange of
                Just range -> [(hRange, range)]
                Nothing -> []
        res <- liftResourceT $
               http req { requestHeaders = reqHeaders } manager
        let resStatus = responseStatus res
        case res of
          _ | resStatus `elem` [ok200, partialContent206] -> do
                let contentType =
                      fromMaybe "application/octet-stream" $
                      hContentType `lookup` responseHeaders res

                    -- |Forward response header
                    forwardHeader name =
                      maybe (return ())
                      (addHeader (decodeUtf8 $ CI.original name) . decodeUtf8) $
                      name `lookup` responseHeaders res
                forwardHeader hContentLength
                forwardHeader hContentRange

                -- Set `Access-Control-Allow-Origin:` response header
                -- if the `Origin:` request header is among the
                -- allowed values
                case mOrigin of
                  Just origin
                    | isOriginAllowed origin -> do
                      addHeader hAccessControlAllowOrigin origin
                      -- Cacheable for 1 day
                      addHeader "Access-Control-Max-Age" "86400"
                  _ ->
                    return ()

                -- Must add statistics upfront before relinquishing
                -- control with `sendResponseStatus`
                case reads <$>
                     BC.unpack <$>
                     hContentLength `lookup` responseHeaders res of
                  Just [(contentLength, "")] ->
                    withDB $ addCounter "up_seeder_w" infoHash contentLength
                  _ ->
                    return ()

                (src, srcFinalizer) <-
                  liftResourceT $
                  unwrapResumable $
                  responseBody res
                liftResourceT $ void $ register $
                  runResourceT srcFinalizer
                let src' = src =$= proxy
                    proxy = do
                      mBuf <- await
                      case mBuf of
                        Nothing ->
                          return ()
                        Just buf -> do
                          yield $ Chunk $
                            fromByteString buf
                          proxy
                sendResponseStatus resStatus (contentType, ContentSource src')
          _ -> do
            liftIO $ putStrLn $
              "Webseed failed with HTTP status " ++ show resStatus
            -- Try next URL
            tryUrl urls'
      tryUrl [] =
        notFound

  tryUrl urls

  where
    userAgent = "Bitlove/0.0 (WebSeeder for WebTorrent support; http://bitlove.org/)"
