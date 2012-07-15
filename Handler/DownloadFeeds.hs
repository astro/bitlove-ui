{-# LANGUAGE TupleSections #-}
module Handler.DownloadFeeds where
-- TODO: absolute links!

import qualified Data.Text as T

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
  
newtype RepRss = RepRss Content

instance HasReps RepRss where
    chooseRep (RepRss content) _cts =
      return (typeRss, content)
      
-- TODO: No ISO8601 perhaps
instance RepFeed RepRss where
  renderFeed params items = do
    let itemLink = "TODO" :: T.Text
        image = pImage params
    RepRss `fmap` hamletToContent [xhamlet|
<rss version="2.0"
     xmlns:atom=#{nsAtom}>
  <channel>
    <title>#{pTitle params}
    <link>@{pLink params}
    $if not (T.null image)
      <image>
        <url>#{image}
    $forall item <- items
      <item>
        <title>#{itemTitle item}
        <link>#{itemLink}
        <guid isPermaLink="true">#{itemLink}
        <published>#{iso8601 (itemPublished item)}
        $if not (T.null $ itemImage item)
            <image>
              <url>#{itemImage item}
        $if not (T.null $ itemPayment item)
            <atom:link rel="payment"
                       href=#{itemPayment item}
                       >
        $forall d <- itemDownloads item
            <link rel="enclosure"
                  type=#{typeTorrent}
                  size=#{downloadSize d}
                  href=@{torrentLink d}
                  >
    |]

newtype RepAtom = RepAtom Content

instance HasReps RepAtom where
    chooseRep (RepAtom content) _cts =
      return (typeAtom, content)
      
instance RepFeed RepAtom where
  renderFeed params items = do
    let itemLink = "TODO" :: T.Text
        image = pImage params
    RepAtom `fmap` hamletToContent [xhamlet|
<feed version="1.0"
      xmlns=#{nsAtom}>
    <title>#{pTitle params}
    <link rel="alternate"
          type="text/html"
          href=@{pLink params}
          >
    <id>@{pLink params}
    $if not (T.null image)
        <link rel="icon"
              href=#{image}
              >
    $forall item <- items
        <title>#{itemTitle item}
        <link rel="alternate"
              type="text/html"
              href=#{itemLink}
              >
        <id>#{itemLink}
        <published>#{iso8601 (itemPublished item)}
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
                  href=@{torrentLink d}
                  >
    |]



getNew :: RepFeed a => Handler a
getNew = withDB (Model.recentDownloads 25) >>=
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
getTop = withDB (Model.popularDownloads 25) >>=
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
  in withDB (Model.mostDownloaded 25 period_days) >>=
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
      downloads <- Model.userDownloads 20 user db
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
        Model.feedDownloads 50 (feedUrl feed) db

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

