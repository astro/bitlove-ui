module Handler.WebSeeder where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
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


getWebSeedR :: HexInfoHash -> Handler (ContentType, Content)
getWebSeedR (HexInfoHash infoHash) = do
  urls <- withDB $ torrentEnclosures infoHash
  liftIO $ putStrLn $
    "Seeding " ++ show infoHash ++
    " with URLs: " ++
    concatMap (\url ->
                 "\n" ++ T.unpack url
              ) urls

  mRange <- lookupHeader hRange

  manager <- httpManager <$> getYesod
  let tryUrl :: [Text] -> Handler (ContentType, Content)
      tryUrl (url:urls') = do
        req <- parseRequest $ T.unpack url
        let reqHeaders =
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
                    forwardHeader name =
                      maybe (return ())
                      (addHeader (decodeUtf8 $ CI.original name) . decodeUtf8) $
                      name `lookup` responseHeaders res
                forwardHeader hContentLength
                forwardHeader hContentRange
                addHeader "Access-Control-Allow-Origin" corsOrigins

                (src, srcFinalizer) <-
                  liftResourceT $
                  unwrapResumable $
                  responseBody res
                let src' = src =$= proxy
                    proxy = do
                      mBuf <- await
                      case mBuf of
                        Nothing ->
                          liftResourceT srcFinalizer
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
    corsOrigins = "http://bitlove.org http://www.bitlove.org https://bitlove.org https://www.bitlove.org"
