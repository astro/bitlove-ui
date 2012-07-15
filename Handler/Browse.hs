{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Handler.Browse where

import qualified Data.Text as T
import Data.Maybe
import Data.Time.Format
import System.Locale
import Network.HTTP.Types (parseQueryText)
import Text.Blaze
import Text.Blaze.Html5 hiding (div, details, map)
import Text.Blaze.Html5.Attributes hiding (item)
import qualified Data.ByteString.Char8 as BC
import Control.Monad

import qualified Model
import Import
import BitloveAuth


getFrontR :: Handler RepHtml
getFrontR = do
  downloads <- withDB $
               Model.mostDownloaded 4 1
  defaultLayout $ do
    setTitle "Bitlove: Peer-to-Peer Love for Your Podcast Downloads"
    $(whamletFile "templates/front.hamlet")
    toWidget [hamlet|
<section class="col2">
  ^{renderDownloads downloads True}
|]

getNewR :: Handler RepHtml
getNewR = do
    downloads <- withDB $
                 Model.recentDownloads 50
    defaultLayout $ do
        setTitle "Bitlove: New Torrents"
        addFilterScript
        let links = [("Downloads", [("RSS", NewRssR, BC.unpack typeRss),
                                    ("ATOM", NewAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        toWidget [hamlet|
<section class="col">
  <h2>New Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
|]

getTopR :: Handler RepHtml
getTopR = do
    downloads <- withDB $
                 Model.popularDownloads 25
    defaultLayout $ do
        setTitle "Bitlove: Popular Torrents"
        addFilterScript
        let links = [("Downloads", [("RSS", TopRssR, BC.unpack typeRss),
                                    ("ATOM", TopAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        toWidget [hamlet|
<section class="col">
  <h2>Popular Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
|]

getTopDownloadedR :: Period -> Handler RepHtml
getTopDownloadedR period = do
  let (period_days, period_title) = 
        case period of
          PeriodDays 1 -> (1, "1 day")
          PeriodDays days -> (days, show days ++ " days")
          PeriodAll -> (10000, "all time")
  downloads <- withDB $
               Model.mostDownloaded 10 period_days
  lift $ lift $ putStrLn $ "render " ++ (show $ length downloads) ++ " downloads"
  defaultLayout $ do
    setTitle "Bitlove: Top Downloaded"
    addFilterScript
    let links = [("Downloads", [("RSS", TopDownloadedRssR period, BC.unpack typeRss),
                                ("ATOM", TopDownloadedAtomR period, BC.unpack typeAtom)
                               ])]
    addFeedsLinks links
    toWidget [hamlet|
<section class="col">
  <h2>Top downloaded in #{period_title}
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
|]

getUserR :: UserName -> Handler RepHtml
getUserR user = do
  canEdit' <- canEdit user
  let fetch =
          withDB $ \db -> do
                  detailss <- Model.userDetailsByName user db
                  case detailss of
                    [] ->
                        return Nothing
                    (details:_) -> 
                        do feeds <- Model.userFeeds user canEdit' db
                           downloads <- Model.userDownloads 20 user db
                           return $ Just (details, feeds, downloads)
      render (details, feeds, downloads) =
          defaultLayout $ do 
                  setTitle $ toMarkup $ userName user `T.append` " on Bitlove"
                  when canEdit' $
                       addScript $ StaticR $ StaticRoute ["edit-user.js"] []
                  let links = [("Downloads", [("RSS", UserDownloadsRssR user, BC.unpack typeRss),
                                              ("ATOM", UserDownloadsAtomR user, BC.unpack typeAtom)
                                             ])]
                  addFeedsLinks links
                  toWidget [hamlet|
<header class="user">
  <div class="meta">
    $if not (T.null $ userImage details)
        <img class="logo"
             src=#{userImage details}>
    <div class="title">
      <h2>#{userTitle details}
      $if not (T.null $ userHomepage details)
          <p class="homepage">
            <a rel="me"
               href=#{userHomepage details}>#{userHomepage details}
<section class="col1">
  <h2>Feeds
  $forall feed <- feeds
    <article class="feed">
      <img class="logo"
           src="#{safeLogo (feedImage feed)}">
      <div>
        <h3>
          <a href="@{UserFeedR user (feedSlug feed)}">#{feedTitle feed}
        $if not (T.null $ feedHomepage feed)
          <p class="homepage">
            <a rel="me"
               href="#{feedHomepage feed}">#{feedHomepage feed}
        $if not (feedPublic feed)
          <p class="hint">Private

<section class="col2">
  <h2>Recent Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads False}
      |]
  
  fetch >>= maybe notFound render
  

getUserFeedR :: UserName -> Text -> Handler RepHtml
getUserFeedR user slug =
  fetch >>= maybe notFound render
    where fetch = 
              withDB $ \db -> do
                feeds <- Model.userFeedInfo user slug db
                case feeds of
                  [] ->
                      return Nothing
                  (feed:_) ->
                      (Just . (feed, )) `fmap` 
                      Model.feedDownloads 50 (feedUrl feed) db
          render (feed, downloads) =
              do canEdit' <- canEdit user
                 defaultLayout $ do
                   setTitle $ toMarkup $ feedTitle feed `T.append` " on Bitlove"
                   when canEdit' $
                        addScript $ StaticR $ StaticRoute ["edit-user.js"] []
                   let links = [("Subscribe",
                                 [("Feed", MapFeedR user slug, BC.unpack typeRss)]),
                                ("Just Downloads", 
                                 [("RSS", UserFeedRssR user slug, BC.unpack typeRss),
                                  ("ATOM", UserFeedAtomR user slug, BC.unpack typeAtom)
                                 ])]
                   addFeedsLinks links
                   toWidget [hamlet|
<section class="col">
  <header class="feed">
    <div class="meta">
      <img class="logo"
           src="#{safeLogo (feedImage feed)}">
      <div class="title">
        <div>
          <h2>#{feedTitle feed}
          <span class="publisher">
            \ by #
            <a href="@{UserR user}">#{userName user}
        $if not (T.null $ feedHomepage feed)
          <p class="homepage">
            <a rel="me"
               href="#{feedHomepage feed}">#{feedHomepage feed}
        $if not (feedPublic feed)
          <p class="hint">
             Private — Not included in directory or public torrent listings

  ^{renderFeedsList links}
  ^{renderDownloads downloads False}
    |]

renderDownloads downloads showOrigin =
    [hamlet|
$forall item <- Model.groupDownloads downloads
  ^{renderItem item showOrigin}
      |]

renderItem item showOrigin =
  let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing ++ " %H:%M") $
             itemPublished item
      isOnlyDownload = length (itemDownloads item) == 1
      bandwidth d
          | downloadDownspeed d < 100 * 1024 =
              Nothing
          | otherwise =
              Just $
              let (n, unit) = humanSize' $ fromIntegral $ downloadDownspeed d
              in (n, unit ++ "B/s")
  in [hamlet|
  <article class="item">
    <div>
      $if not (T.null $ itemImage item)
        <img src="#{itemImage item}" class="logo">
      <div class="right">
        <p class="published">#{date}
        $if not (T.null $ itemPayment item)
          <div class="flattr">
            #{renderPayment}
      <div class="title">
        <h3>
          <a href="@{UserFeedR (itemUser item) (itemSlug item)}##{itemId item}">#{itemTitle item}
        $if showOrigin
          <p class="feed">
            \ in #
            <a href="@{UserFeedR (itemUser item) (itemSlug item)}">#{fromMaybe (itemSlug item) $ itemFeedTitle item}
            \ by #
            <a href="@{UserR $ itemUser item}">#{userName $ itemUser item}
    $forall d <- itemDownloads item
      <ul class="download">
        <li class="torrent">
          <a href=@{TorrentFileR (downloadUser d) (downloadSlug d) (TorrentName $ downloadName d)}
             rel="enclosure" data-type=#{downloadType d}>
            $if isOnlyDownload
              <span>Download
            $else
              <span>#{downloadName d}
            \ #
            <span class="size" title="Download size">
              #{humanSize (downloadSize d)}
        <li class="stats">
          $maybe (speed, unit) <- bandwidth d
              <dl>
                <dt>
                  #{speed}
                  <span class="unit"> #{unit}
                <dd>Download speed
                  
          <dl class="seeders">
            <dt>#{downloadSeeders d}
            <dd>Seeders
          <dl class="leechers">
            <dt>#{downloadLeechers d}
            <dd>Leechers
          <dl class="downloads">
            <dt>#{downloadDownloaded d}
            <dd>Downloads
|]
  where payment = itemPayment item
        homepage = itemHomepage item
        renderPayment
            | "https://flattr.com/submit/auto?" `T.isPrefixOf` payment =
                let qs = mapMaybe (\(k, mv) ->
                                     (k, ) `fmap` mv
                                  ) $
                         parseQueryText $
                         BC.pack $
                         T.unpack $
                         snd $
                         T.break (== '?') $
                          payment
                    qs' = ("popout", "0") :
                          filter ((/= "popout") . fst) qs
                in foldl (\tag (k, v) ->
                              let ma | k == "user_id" =
                                         Just "uid"
                                     | all (\c ->
                                                c >= 'a' && c <= 'z'
                                           ) $ T.unpack k =
                                       Just k
                                     | otherwise =
                                         Nothing
                              in maybe tag
                                     (\k' ->
                                          tag
                                          ! dataAttribute (textTag $ "flattr-" `T.append` k') (toValue v)
                                     ) ma
                         ) (a
                            ! class_ "FlattrButton"
                            ! rel "payment"
                            ! href (toValue payment)
                            $ "[Flattr]") qs'
            | "http://flattr.com/" `T.isPrefixOf` payment ||
              "https://flattr.com/" `T.isPrefixOf` payment =
                a ! class_ "FlattrButton"
                  ! rel "payment"
                  ! href (toValue payment)
                  ! dataAttribute "flattr-url" (toValue homepage)
                  ! dataAttribute "flattr-popout" "0" $
                  "[Flattr]"
            | otherwise =
                a ! rel "payment"
                  ! href (toValue payment) $
                  "[Support]"

addFilterScript =
    addScript $ StaticR $ StaticRoute ["filter.js"] []

-- | <link rel="alternate"> to <head>
addFeedsLinks lists =
    addHamletHead
    [hamlet|
     $forall feed <- concat $ map snd lists
       ^{addFeedsLink feed}
     |]
    where addFeedsLink (title :: Text, route, type_) =
            [hamlet|
             <link rel="alternate"
                   type="#{type_}"
                   href="http://bitlove.org@{route}"
                   title="#{title}">
             |]

renderFeedsList lists =
    [hamlet|
     <dl class="feedslist">
       $forall list <- lists
         ^{renderFeedsList' list}
     |]
    where renderFeedsList' (title :: Text, feeds) =
              [hamlet|
               <dt>#{title}:
               $forall feed <- feeds
                 <dd>^{renderFeedsList'' feed}
               |]
          renderFeedsList'' (title :: Text, route, type_) =
              [hamlet|
               <a href="@{route}"
                  type=#{type_}>#{title}
               |]

safeLogo :: Text -> Text
safeLogo url
    | "http" `T.isPrefixOf` url = url
    | otherwise = "/static/stub.png"

humanSize :: (Integral a, Show a) => a -> String
humanSize n = let (n', unit) = humanSize' $ fromIntegral n
                  ns | n' < 10 = show $
                                 fromIntegral (truncate $ n' * 10) / 10
                     | otherwise = show $ truncate n'
              in ns ++ " " ++ unit ++ "B"

humanSize' :: Double -> (Double, String)
humanSize' n = foldl (\(n', unit) unit' ->
                          if n' < 1024
                          then (n', unit)
                          else (n' / 1024, [unit'])
                     ) (n, "") "KMGT"
