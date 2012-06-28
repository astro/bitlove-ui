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
  ^{renderDownloads downloads}
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
  ^{renderDownloads downloads}
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
  ^{renderDownloads downloads}
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
  ^{renderDownloads downloads}
^{filterScript}
|]

getUserR :: Text -> Handler RepHtml
getUserR user = do
    (details, downloads) <- withDB $ \db -> do
      details <- Model.userDetailsByName user db
      downloads <- Model.userDownloads 20 user db
      return (details, downloads)
    case details of
      [] ->
        notFound
      (details':_) ->
        defaultLayout $ do
          setTitle $ toMarkup $ user `T.append` " on Bitlove"
          toWidget [hamlet|
<header class="user">
  <div class="meta">
    $if not (T.null $ userImage details')
        <img class="logo"
             src=#{userImage details'}>
    <div class="title">
      <h2>#{userTitle details'}
      $if not (T.null $ userHomepage details')
          <p class="homepage">
            <a rel="me"
               href=#{userHomepage details'}>#{userHomepage details'}
<section class="col1">
  <h2>Feeds
<section class="col2">
  <h2>Recent Torrents
  ^{renderDownloads downloads}
|]

renderDownloads downloads = 
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
          <a href="">#{itemTitle item}
        <p class="feed">
          \ in #
          <a href="">#{fromMaybe T.empty $ itemFeedTitle item}
          \ by #
          <a href="">#{itemUser item}
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
               