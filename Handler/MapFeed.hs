module Handler.MapFeed where

import Control.Monad
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.Stream.Parse
import Data.XML.Types
import Text.XML.Stream.Render
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy.Char8 as LBC
import Blaze.ByteString.Builder (Builder)
import Data.Maybe

import qualified Model
import Import hiding (Content)

getMapFeedR :: Text -> Text -> Handler RepXml
getMapFeedR user slug = do
  urlRender <- getUrlRender
  m_data <- withDB $ \db -> do
    urls <- Model.user_feed user slug db
    case urls of
      (url:_) -> do
        xml:_ <- Model.feedXml url db
        enclosures <- Map.fromList `fmap`
                      Model.feedEnclosures url db
        let getEnclosureLink url = do 
                              name <- url `Map.lookup` enclosures
                              -- TODO: full url!
                              return $ urlRender $ TorrentFileR user slug (TorrentName name)
        return $ Just (xml, getEnclosureLink)
      _ ->
        return Nothing
  case m_data of
    Nothing ->
      notFound
    Just (xml, enclosures) ->
      return $ RepXml $ ContentSource $
      mapFeed xml enclosures
  
  
type Attrs = [(Name, [Content])]
  
mapFeed :: FeedXml -> (Text -> Maybe Text) -> Source (ResourceT IO) (Flush Builder)
mapFeed (FeedXml xml) getEnclosureLink =
  parseLBS def xml $=
  mapEnclosures =$=
  renderBuilder def =$=
  CL.map Chunk
  where mapEnclosures :: Monad m => Conduit Event m Event
        mapEnclosures =
          CL.map (\event ->
                   case event of
                     EventBeginElement name@(Name {
                                               nameLocalName = "link",
                                               nameNamespace = Just xmlnsAtom
                                             }) attrs 
                         | (attrs `getAttr` "rel") == "enclosure" ->
                             let href = attrs `getAttr` "href"
                                 attrs' = updateAttr attrs "type" $ Just typeTorrent
                             in EventBeginElement name $
                                updateAttr attrs' "href" $ getEnclosureLink href
                     EventBeginElement name@(Name {
                                                nameLocalName = "enclosure",
                                                nameNamespace = Nothing
                                             }) attrs ->
                         let url = attrs `getAttr` "url"
                             attrs' = updateAttr attrs "type" $ Just typeTorrent
                         in EventBeginElement name $
                            updateAttr attrs' "url" $ getEnclosureLink url
                     EventBeginElement name attrs ->
                         event
                     _ ->
                         event
                 )
        getAttr :: Attrs -> Name -> T.Text
        getAttr attrs name = 
          contentsToText $
          fromMaybe [] $
          name `lookup` attrs
        contentsToText :: [Content] -> Text
        contentsToText =
            T.concat . map toText
                where
                  toText (ContentText t) = t
                  toText (ContentEntity e) = T.concat ["&", e, ";"]
        updateAttr :: Attrs -> Name -> Maybe T.Text -> Attrs
        updateAttr attrs name m_val =
          let attrs' = filter (\(name', _) ->
                                name /= name'
                              ) attrs
          in case m_val of
               Just val -> (name, [ContentText val]) : attrs'
               Nothing -> attrs'

typeTorrent = "application/x-bittorrent"
xmlnsAtom = "http://www.w3.org/2005/Atom"
