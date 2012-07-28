{-# LANGUAGE TupleSections #-}
module Handler.DownloadFeeds where

import qualified Data.Text as T
import Data.Maybe
import Data.Time (getCurrentTimeZone)
import Data.Default (def)
import Blaze.ByteString.Builder

import qualified Model
import Import


typeTorrent :: T.Text
typeTorrent = "application/x-bittorrent"

nsAtom :: T.Text
nsAtom = "http://www.w3.org/2005/Atom"

torrentLink :: Download -> Route UIApp
torrentLink d = TorrentFileR 
                (downloadUser d) 
                (downloadSlug d) 
                (TorrentName $ downloadName d)

data FeedParameters = Parameters {
      pTitle :: T.Text,
      pImage :: T.Text,
      pLink :: Route UIApp
    }

class RepFeed c where
  renderFeed :: FeedParameters -> [Item] -> Handler c
  
  renderFeed' :: FeedParameters -> [Download] -> Handler c
  renderFeed' params downloads = renderFeed params $
                                 groupDownloads downloads
  
withXmlDecl :: Content -> Content
withXmlDecl (ContentBuilder b _) =
  flip ContentBuilder Nothing $
  fromByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n" `mappend`
  b
withXmlDecl c = c

newtype RepRss = RepRss Content

instance HasReps RepRss where
    chooseRep (RepRss content) _cts =
      return (typeRss, withXmlDecl content)
      
instance RepFeed RepRss where
  renderFeed params items = do
    url <- getFullUrlRender
    let image = pImage params
    RepRss `fmap` hamletToContent [xhamlet|
<rss version="2.0"
     xmlns:atom=#{nsAtom}>
  <channel>
    <title>#{pTitle params}
    <link>#{url $ pLink params}
    $if not (T.null image)
      <image>
        <url>#{image}
    $forall item <- items
      <item>
        <title>#{itemTitle item}
        <link>#{itemLink url item}
        $if not (T.null $ fromMaybe "" $ itemLang item)
          <language>#{fromMaybe "" $ itemLang item}
        $maybe summary <- itemSummary item
          <description>#{summary}
        <guid isPermaLink="true">#{itemLink url item}
        <pubDate>#{rfc822 (itemPublished item)}
        $if not (T.null $ itemImage item)
            <image>
              <url>#{itemImage item}
        $if not (T.null $ itemPayment item)
            <atom:link rel="payment"
                       href=#{itemPayment item}
                       >
        $forall d <- itemDownloads item
            <link rel="enclosure"
                  type="#{typeTorrent}"
                  size="#{downloadSize d}"
                  href="#{url $ torrentLink d}">
    |]

newtype RepAtom = RepAtom Content

instance HasReps RepAtom where
    chooseRep (RepAtom content) _cts =
      return (typeAtom, withXmlDecl content)
      
instance RepFeed RepAtom where
  renderFeed params items = do
    let image = pImage params
    url <- getFullUrlRender
    tz <- liftIO getCurrentTimeZone
    RepAtom `fmap` hamletToContent [xhamlet|
<feed version="1.0"
      xmlns=#{nsAtom}>
    <title>#{pTitle params}
    <link rel="alternate"
          type="text/html"
          href=#{url $ pLink params}
          >
    <id>#{url $ pLink params}
    $if not (T.null image)
        <link rel="icon"
              href=#{image}
              >
    $forall item <- items
      <entry xml:lang="#{fromMaybe "" $ itemLang item}">
        <title>#{itemTitle item}
        <link rel="alternate"
              type="text/html"
              href=#{itemLink url item}
              >
        <id>#{itemLink url item}
        <published>#{iso8601 $ localTimeToZonedTime tz $ itemPublished item}
        $maybe summary <- itemSummary item
          <summary>#{summary}
        $if not (T.null $ itemImage item)
            <link rel="icon"
                  href=#{itemImage item}
                  >

        $if not (T.null $ itemPayment item)
            <link rel="payment"
                  href=#{itemPayment item}
                  >
        $forall d <- itemDownloads item
            <link rel="enclosure"
                  type=#{typeTorrent}
                  size=#{downloadSize d}
                  href=#{url $ torrentLink d}
                  >
    |]

itemLink urlRender item =
    urlRender (UserFeedR (itemUser item) (itemSlug item)) `T.append`
    "#" `T.append`
    itemId item

getNew :: RepFeed a => Handler a
getNew = withDB (Model.recentDownloads def) >>=
         renderFeed' Parameters {
                           pTitle = "Bitlove: New",
                           pLink = NewR,
                           pImage = ""
                         }

getNewRssR :: Handler RepRss
getNewRssR = getNew

getNewAtomR :: Handler RepAtom
getNewAtomR = getNew

getTop :: RepFeed a => Handler a
getTop = withDB (Model.popularDownloads def) >>=
         renderFeed' Parameters {
                           pTitle = "Bitlove: Top",
                           pLink = TopR,
                           pImage = ""
                         }
         
getTopRssR :: Handler RepRss
getTopRssR = getTop

getTopAtomR :: Handler RepAtom
getTopAtomR = getTop

getTopDownloaded :: RepFeed a => Period -> Handler a
getTopDownloaded period = 
  let (period_days, period_title) = 
        case period of
          PeriodDays 1 -> (1, "1 day")
          PeriodDays days -> (days, T.pack $ show days ++ " days")
          PeriodAll -> (10000, "all time")
  in withDB (Model.mostDownloaded period_days def) >>=
         renderFeed' Parameters {
                           pTitle = "Bitlove: Top Downloaded in " `T.append` period_title,
                           pLink = TopDownloadedR period,
                           pImage = ""
                         }
         
getTopDownloadedRssR :: Period -> Handler RepRss
getTopDownloadedRssR = getTopDownloaded

getTopDownloadedAtomR :: Period -> Handler RepAtom
getTopDownloadedAtomR = getTopDownloaded

getUserDownloads :: RepFeed a => UserName -> Handler a
getUserDownloads user = do
    (details, downloads) <- withDB $ \db -> do
      details <- Model.userDetailsByName user db
      downloads <- Model.userDownloads user def db
      return (details, downloads)
    case details of
      [] ->
        notFound
      (details':_) ->
          renderFeed' Parameters {
                                pTitle = userName user `T.append` " on Bitlove",
                                pLink = UserR user,
                                pImage = userImage details'
                              } downloads
    
getUserDownloadsRssR :: UserName -> Handler RepRss
getUserDownloadsRssR = getUserDownloads

getUserDownloadsAtomR :: UserName -> Handler RepAtom
getUserDownloadsAtomR = getUserDownloads


getUserFeed :: RepFeed a => UserName -> Text -> Handler a
getUserFeed user slug = do
  mFeedDownloads <- withDB $ \db -> do
    feeds <- Model.userFeedInfo user slug db
    case feeds of
      [] ->
        return Nothing
      (feed:_) ->
        (Just . (feed, )) `fmap` 
        Model.feedDownloads (feedUrl feed) def db

  case mFeedDownloads of
    Nothing ->
        notFound
    Just (feed, downloads) ->
        renderFeed' Parameters 
                      { pTitle = feedTitle feed `T.append` " on Bitlove"
                      , pLink = UserFeedR user slug
                      , pImage = feedImage feed 
                      } downloads

getUserFeedRssR :: UserName -> Text -> Handler RepRss
getUserFeedRssR = getUserFeed

getUserFeedAtomR :: UserName -> Text -> Handler RepAtom
getUserFeedAtomR = getUserFeed

