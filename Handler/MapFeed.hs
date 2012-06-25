module Handler.MapFeed where

import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.Stream.Parse
import Data.XML.Types (Event (..))
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy.Char8 as LBC

import qualified Model
import Import

getMapFeedR :: Text -> Text -> Handler RepXml
getMapFeedR user slug = do
  m_data <- withDB $ \db -> do
    urls <- Model.user_feed user slug db
    case urls of
      (url:_) -> do
        xml:_ <- Model.feedXml url db
        enclosures <- Map.fromList `fmap` Model.feedEnclosures url db
        return $ Just (xml, enclosures)
      _ ->
        return Nothing
  case m_data of
    Nothing ->
      notFound
    Just (xml, enclosures) ->
      lift $
      (RepXml . toContent) `fmap`
      mapFeed xml enclosures
  
  
mapFeed :: MonadThrow m => FeedXml -> Map Text Text -> ResourceT m LBC.ByteString
mapFeed (FeedXml xml) enclosures =
  yield xml $=
  parseText def $$
  CL.map (\event ->
           event
         ) =$
  eventsToXml
  
data ToXmlState = XmlStateText
                | XmlStateOpen
  
eventsToXml :: Monad m => Sink Event m LBC.ByteString
eventsToXml =
  sinkState XmlStateText
  (\state event ->
    return $ SinkStateResult state LBC.empty
  )
  (\state ->
    return LBC.empty
  )