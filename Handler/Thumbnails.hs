{-# LANGUAGE DeriveDataTypeable #-}
module Handler.Thumbnails where

import Data.Conduit
import Network.HTTP.Conduit
import System.Process.QQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.HTTP.Types (statusCode)
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Maybe
import qualified Control.Exception.Lifted as EX
import System.IO
import Control.Monad
import Data.Data (Typeable)
import Control.Monad.Trans.Resource

import PathPieces
import Import
import qualified Model
import qualified Model.ImageCache as Cache


generateThumbnail :: T.Text -> Int -> Handler  (Maybe LB.ByteString)
generateThumbnail url size = 
    do cacheSeconds $ 24 * 60 * 60
       manager <- httpManager <$> getYesod
       lift $ runResourceT $
            do mSource <- download manager
               case mSource of
                 Nothing ->
                     return Nothing
                 Just source ->
                     Just <$> LB.fromChunks <$>
                     (source $$+- resize =$ CL.consume)
  where download :: Manager -> ResourceT IO (Maybe (ResumableSource (ResourceT IO) B.ByteString))
        download manager = do
          req <- parseRequest $ T.unpack url
          res <- http req manager
          case res of
            _ | statusCode (responseStatus res) == 200 ->
                return $ Just $ responseBody res
            _ ->
                return Nothing
        resize :: Conduit B.ByteString (ResourceT IO) B.ByteString
        resize = [ccmd|
                  mogrify -thumbnail #{show size}x#{show size} -format png -
                  |]


newtype RepPng = RepPng LB.ByteString

instance ToContent RepPng where
    toContent (RepPng b) =
        let builder = fromLazyByteString b
            len = fromIntegral $ LB.length b
        in ContentBuilder builder $ Just len

instance ToTypedContent RepPng where
    toTypedContent = TypedContent typePng . toContent

data EmptyException = EmptyException
                    deriving (Show, Typeable)

instance EX.Exception EmptyException

serveThumbnail :: Int -> Text -> Handler RepPng
serveThumbnail size url
    | T.null url = noThumbnail
    | otherwise =
        do cacheSeconds (24 * 60 * 60)
           
           mImg <- listToMaybe <$> withDB (Cache.getImage url size)
           case mImg of
             Just (Cache.CachedImage data_) ->
                 return $ RepPng $ data_
             Just (Cache.CachedError _) ->
                 noThumbnail
             Nothing ->
                 do let storeReturn data_ =
                            do when (LB.null data_) $
                                    EX.throw EmptyException
                               -- TODO: catch here for concurrent updates
                               withDB $ Cache.putImage url size $
                                      Cache.CachedImage data_
                               return $ RepPng $ data_
                        bail :: EX.SomeException -> Handler a
                        bail = bail' . show
                        bail' :: String -> Handler a
                        bail' e = do 
                          liftIO $ hPutStrLn stderr $ 
                                     "Image cache " ++ show url ++ 
                                     " " ++ e
                          withDB $ Cache.putImage url size $ 
                                 Cache.CachedError $ T.pack $ show e
                          noThumbnail
                    EX.catch (generateThumbnail url size >>=
                              maybe (bail' "No HTTP source") storeReturn
                             ) bail
      
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
