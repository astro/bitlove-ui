module Handler.Thumbnails where

import Data.Conduit
import Network.HTTP.Conduit
import System.Process.QQ
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Types (statusCode)
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (fromByteString)

import PathPieces
import Import
import qualified Model


generateThumbnail :: T.Text -> Int -> Handler (Maybe (Source (ResourceT IO) B.ByteString))
generateThumbnail url size =
    (maybe Nothing $ Just . ($= resize)) <$>
    download
  where download :: Handler (Maybe (Source (ResourceT IO) B.ByteString))
        download = do
          manager <- httpManager <$> getYesod
          req <- lift $ parseUrl $ T.unpack url
          res <- lift $ http req manager
          case res of
            Response _ _ _ src
              | statusCode (responseStatus res) == 200 ->
                return $ Just src
            _ ->
              return Nothing
        resize :: Conduit B.ByteString (ResourceT IO) B.ByteString
        resize = [ccmd|
                  mogrify -thumbnail #{show size}x#{show size} -format png -
                  |]


newtype RepPng = RepPng (Source (ResourceT IO) B.ByteString)

instance HasReps RepPng where
  chooseRep (RepPng src) _cts =
    return (typePng,
            ContentSource $ src =$= CL.map (Chunk . fromByteString))


getUserThumbnailR :: Model.UserName -> Thumbnail -> Handler RepPng
getUserThumbnailR user (Thumbnail size) = do
  mUrl <-
      withDB $ \db -> do
        detailss <- Model.userDetailsByName user db
        case detailss of
          [] ->
              return Nothing
          (details:_) -> 
              -- TODO: cache
              return $ Just $ userImage details
  case mUrl of
    Nothing ->
        -- TODO: redirect to stub.png
        notFound
    Just url ->
        generateThumbnail url size >>=
        maybe notFound (return . RepPng)
  
getUserFeedThumbnailR :: Model.UserName -> Text -> Thumbnail -> Handler RepPng
getUserFeedThumbnailR = undefined

getUserFeedItemThumbnailR :: Model.UserName -> Text -> Text -> Thumbnail -> Handler RepPng
getUserFeedItemThumbnailR = undefined
