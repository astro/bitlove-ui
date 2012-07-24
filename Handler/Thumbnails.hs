module Handler.Thumbnails where

import Data.Conduit
import Network.HTTP.Conduit
import System.Process.QQ
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Types (statusCode)
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (fromByteString)
import Data.Maybe
import qualified Control.Exception.Lifted as EX

import PathPieces
import Import
import qualified Model
import qualified Model.ImageCache as Cache


generateThumbnail :: T.Text -> Int -> Handler (Maybe (Source (ResourceT IO) B.ByteString))
generateThumbnail url size = 
    do cacheSeconds $ 24 * 60 * 60
       maybe Nothing (Just . ($= resize)) <$> download
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


newtype RepPng = RepPng B.ByteString

instance HasReps RepPng where
  chooseRep (RepPng src) _cts =
    return (typePng,
            ContentBuilder (fromByteString src) (Just $ B.length src)
           )

serveThumbnail :: Int -> Text -> Handler RepPng
serveThumbnail size url
    | T.null url = noThumbnail
    | otherwise =
        do cacheSeconds (24 * 60 * 60)
           
           mImg <- listToMaybe <$> withDB (Cache.getImage url size)
           case mImg of
             Just (Cache.CachedImage data_) ->
                 return $ RepPng $ data_
             Nothing ->
                 do mSrc <- generateThumbnail url size
                    case mSrc of
                      Nothing ->
                          noThumbnail
                      Just src ->
                          let fetchStoreReturn =
                                  do data_ <- lift $ B.concat <$> (src $$ CL.consume)
                                     withDB $ Cache.putImage url size data_
                                     return $ RepPng $ data_
                              bail :: EX.SomeException -> Handler a
                              bail = const noThumbnail
                          in EX.catch fetchStoreReturn bail
      
noThumbnail :: Handler a
noThumbnail = redirect $ StaticR $ StaticRoute ["stub.png"] []

getUserThumbnailR :: Model.UserName -> Thumbnail -> Handler RepPng
getUserThumbnailR user (Thumbnail size) =
  (fromMaybe "" . (userImage <$>) . listToMaybe) <$> withDB (Model.userDetailsByName user) >>=
  serveThumbnail size
  
getUserFeedThumbnailR :: Model.UserName -> Text -> Thumbnail -> Handler RepPng
getUserFeedThumbnailR user slug (Thumbnail size) =
  (fromMaybe "" . (feedImage <$>) . listToMaybe) <$> withDB (Model.userFeedInfo user slug) >>=
  serveThumbnail size

getUserFeedItemThumbnailR :: Model.UserName -> Text -> Text -> Thumbnail -> Handler RepPng
getUserFeedItemThumbnailR user slug item (Thumbnail size) =
  (fromMaybe "" . listToMaybe) <$> withDB (Model.userItemImage user slug item) >>=
  serveThumbnail size
