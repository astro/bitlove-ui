module Handler.WebSeeder where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Yesod
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.Header
import Data.Binary.Builder (fromByteString)

import Prelude
import Foundation
import PathPieces
import Model.Download (torrentEnclosures)

-- |TODO: Content-Length, Range, CORS
getWebSeedR :: HexInfoHash -> Handler (ContentType, Content)
getWebSeedR (HexInfoHash infoHash) = do
  urls <- withDB $ torrentEnclosures infoHash
  liftIO $ putStrLn $
    "Seeding " ++ show infoHash ++
    " with URLs: " ++
    concatMap (\url ->
                 "\n" ++ T.unpack url
              ) urls

  manager <- httpManager <$> getYesod
  let tryUrl :: [Text] -> Handler (ContentType, Content)
      tryUrl (url:urls) = do
        req <- parseRequest $ T.unpack url
        res <- liftResourceT $
               http req manager
        case res of
          _ | statusCode (responseStatus res) == 200 -> do
                let contentType =
                      fromMaybe "application/octet-stream" $
                      hContentType `lookup` responseHeaders res

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
                return (contentType, ContentSource src')
          _ ->
            -- Try next URL
            tryUrl urls
      tryUrl [] =
        notFound

  tryUrl urls
