{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Browse where

import qualified Data.Text as T
import Data.Maybe
import Data.Time.Format
import System.Locale
import Text.Blaze (toMarkup)
import Network.HTTP.Types (parseQueryText)
import Text.Blaze
import Text.Blaze.Html5 hiding (div)
import Text.Blaze.Html5.Attributes
import qualified Data.ByteString.Char8 as BC

import qualified Model
import Import
import BitloveAuth


getFrontR :: Handler RepHtml
getFrontR = do
  downloads <- withDB $
               Model.mostDownloaded 4 1
  --let text = $(whamletFile "templates/front.hamlet")
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
        toWidget [hamlet|
<section class="col">
  <h2>New Torrents
  ^{renderDownloads downloads True}
^{filterScript}
|]

getTopR :: Handler RepHtml
getTopR = do
    downloads <- withDB $
                 Model.popularDownloads 25
    defaultLayout $ do
        setTitle "Bitlove: Popular Torrents"
        toWidget [hamlet|
<section class="col">
  <h2>Popular Torrents
  ^{renderDownloads downloads True}
^{filterScript}
|]

getTopDownloadedR :: Period -> Handler RepHtml
getTopDownloadedR period = do
  let (p, period_days) = 
        case period of
          PeriodDays 1 -> (1, "1 day")
          PeriodDays days -> (days, show days ++ " days")
          PeriodAll -> (10000, "all time")
  downloads <- withDB $
               Model.mostDownloaded 10 p
  lift $ lift $ putStrLn $ "render " ++ (show $ length downloads) ++ " downloads"
  defaultLayout $ do
    setTitle "Bitlove: Top Downloaded"
    toWidget [hamlet|
<section class="col">
  <h2>Top downloaded in #{period_days}
  ^{renderDownloads downloads True}
^{filterScript}
|]

getUserR :: UserName -> Handler RepHtml
getUserR user =
  lookup >>= maybe notFound render
    where lookup =
              withDB $ \db -> do
                detailss <- Model.userDetailsByName user db
                case detailss of
                  [] ->
                      return Nothing
                  (details:_) -> 
                      do
                        feeds <- Model.userFeeds user False db
                        downloads <- Model.userDownloads 20 user db
                        return $ Just (details, feeds, downloads)
          render (details, feeds, downloads) =
              do msu <- sessionUser
                 defaultLayout $ do 
                   setTitle $ toMarkup $ userName user `T.append` " on Bitlove"
                   case msu of
                     Nothing -> 
                         return ()
                     Just user' | user == user' ->
                         toWidgetHead [hamlet|
                                       <script src="/static/edit-user.js" type="text/javascript" async>
                                       |]
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

<section class="col2">
  <h2>Recent Torrents
  ^{renderDownloads downloads False}
      |]

getUserFeedR :: UserName -> Text -> Handler RepHtml
getUserFeedR user slug =
  lookup >>= maybe notFound render
    where lookup = 
              withDB $ \db -> do
                feeds <- Model.userFeedDetails user slug db
                case feeds of
                  [] ->
                      return Nothing
                  (feed:_) ->
                      (Just . (feed, )) `fmap` 
                      Model.feedDownloads 50 (feedUrl feed) db
          render (feed, downloads) =
              do msu <- sessionUser
                 defaultLayout $ do
                   setTitle $ toMarkup $ feedTitle feed `T.append` " on Bitlove"
                   case msu of
                     Nothing -> 
                         return ()
                     Just user' | user == user' ->
                         toWidgetHead [hamlet|
                                       <script src="/static/edit-feed.js" type="text/javascript" async>
                                       |]
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
                          filter (\(k, v) ->
                                      k /= "url" &&
                                      k /= "popout"
                                 ) qs
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
                            ! dataAttribute "flattr-url" (toValue homepage)
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

safeLogo url
    | "http" `T.isPrefixOf` url = url
    | otherwise = "/static/stub.png"

filterScript = [hamlet|
<script src="/static/filter.js" type="text/javascript">
    |]


humanSize = humanSize' "KMGT" ""
  where humanSize' units unit n
          | n < 1024 || null units = 
            show n ++ " " ++ unit ++ "B"
          | otherwise = 
            let (unit':units') = units
            in humanSize' units' [unit'] $ n `div` 1024
