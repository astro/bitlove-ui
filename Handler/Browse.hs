{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Browse where

import qualified Data.Text as T
import Data.Maybe
import Data.Time.Format
import System.Locale
import Text.Blaze (toMarkup)

import qualified Model
import Import


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

getUserR :: Text -> Handler RepHtml
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
              defaultLayout $ do 
                setTitle $ toMarkup $ user `T.append` " on Bitlove"
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

getUserFeedR :: Text -> Text -> Handler RepHtml
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
              defaultLayout $ do
                setTitle $ toMarkup $ feedTitle feed `T.append` " on Bitlove"
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
            <a href="@{UserR user}">#{user}
        $if not (T.null $ feedHomepage feed)
          <p class="homepage">
            <a rel="me"
               href="#{feedHomepage feed}">#{feedHomepage feed}

  ^{renderDownloads downloads False}
    |]
  

renderDownloads downloads showOrigin = 
  let formatDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing ++ " %H:%M") .
                   itemPublished
      isOnlyDownload = (== 1) . length . itemDownloads
  in [hamlet|
$forall item <- Model.groupDownloads downloads
  <article class="item">
    <div>
      $if not (T.null $ itemImage item)
        <img src="#{itemImage item}" class="logo">
      <div class="right">
        <p class="published">#{formatDate item}
        <div class="flattr">
      <div class="title">
        <h3>
          <a href="@{UserFeedR (itemUser item) (itemSlug item)}##{itemId item}">#{itemTitle item}
        $if showOrigin
          <p class="feed">
            \ in #
            <a href="@{UserFeedR (itemUser item) (itemSlug item)}">#{fromMaybe (itemSlug item) $ itemFeedTitle item}
            \ by #
            <a href="@{UserR $ itemUser item}">#{itemUser item}
    $forall d <- itemDownloads item
      <ul class="download">
        <li class="torrent">
          <a href=@{TorrentFileR (downloadUser d) (downloadSlug d) (TorrentName $ downloadName d)}
             rel="enclosure" data-type=#{downloadType d}>
            $if isOnlyDownload item
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
               