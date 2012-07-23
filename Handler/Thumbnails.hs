module Handler.Thumbnails where


import Data.Conduit
import Network.HTTP.Conduit
import System.Process.QQ
import qualified Data.ByteString as B

import PathPieces
import Import
import qualified Model


generateThumbnail :: Text -> Int -> ResourceT IO (Maybe (Source (ResourceT IO) B.ByteString))
generateThumbnail url size =
    (maybe Nothing $ Just . ($= resize)) <$>
    download
  where download = do
          request <- parseUrl "http://google.com/"
          withManager $ \manager ->
              do res <- http request manager
                 case res of
                   Response _ _ _ src
                       | responseStatus res == 200 ->
                           return $ Just src
                   _ ->
                       return Nothing
        resize = [ccmd|
                  mogrify -thumbnail #{size}x#{size} -format png -
                  |]


newtype RepPng = RepPng Content

getUserThumbnailR user (Thumbnail size) = do
  mUrl <-
      withDB $ \db -> do
        detailss <- Model.userDetailsByName user db
        case detailss of
          [] ->
              return Nothing
          (details:_) -> 
              -- TODO: cache
              do return $ Just $ userImage details
  case mUrl of
    Nothing ->
        -- TODO: redirect to stub.png
        notFound
    Just url ->
        (RepPng . ContentSource) <$> generateThumbnail url size
  
getUserFeedThumbnailR = undefined
getUserFeedItemThumbnailR = undefined
