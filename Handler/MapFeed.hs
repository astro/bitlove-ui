module Handler.MapFeed where

import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Text.XML.Stream.Parse
import Data.XML.Types
import Text.XML.Stream.Render
import Data.Conduit
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (Builder)
import Data.Maybe

import qualified Model
import Import hiding (Content)

getMapFeedR :: UserName -> Text -> Handler RepXml
getMapFeedR user slug = do
  fullUrlRender <- getFullUrlRender
  m_data <- withDB $ \db -> do
    urls <- Model.userFeed user slug db
    case urls of
      (url:_) -> do
        xml:_ <- Model.feedXml url db
        enclosures <- HM.fromList `fmap`
                      Model.feedEnclosures url db
        let getEnclosureLink enclosure = 
                (fullUrlRender . TorrentFileR user slug . TorrentName) `fmap`
                ({-# SCC "enclosureLookup" #-} enclosure `HM.lookup` enclosures)
        return $ Just (xml, enclosures `seq` getEnclosureLink)
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
                                               nameNamespace = Just ns
                                             }) attrs 
                         | ns == xmlnsAtom &&
                           (attrs `getAttr` "rel") == "enclosure" ->
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
                     EventBeginElement _ _ ->
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
          let attrs' = filter ((name /=) . fst) attrs
          in case m_val of
               Just val -> (name, [ContentText val]) : attrs'
               Nothing -> attrs'

typeTorrent :: Text
typeTorrent = "application/x-bittorrent"

xmlnsAtom :: Text
xmlnsAtom = "http://www.w3.org/2005/Atom"
