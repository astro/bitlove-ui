{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Handler.Browse where

import qualified Data.Text as T
import Data.Maybe
import Data.Time.Format
import System.Locale
import Network.HTTP.Types (parseQueryText)
import Text.Blaze
import Text.Blaze.Html5 hiding (div, details, map)
import Text.Blaze.Html5.Attributes hiding (item, min, max, id)
import qualified Data.ByteString.Char8 as BC
import Control.Monad

import PathPieces
import qualified Model
import Import
import BitloveAuth

data Page = Page { pageNumber :: Int
                 , pagePrevious :: Maybe Text
                 , pageNext :: Maybe Text
                 }

pageSize = 30

withPage :: Page -> (Model.QueryPage -> a) -> a
withPage p f =
    f $ Model.QueryPage pageSize $ (pageNumber p - 1) * pageSize


getFrontR :: Handler RepHtml
getFrontR = do
  downloads <- withDB $
               Model.mostDownloaded 1 (Model.QueryPage 4 0)
  defaultLayout $ do
    setTitle "Bitlove: Peer-to-Peer Love for Your Podcast Downloads"
    $(whamletFile "templates/front.hamlet")
    toWidget [hamlet|
<section class="col2">
  ^{renderDownloads downloads True}
|]

getNewR :: Handler RepHtml
getNewR = do
    page <- makePage
    downloads <- withDB $
                 withPage page $
                 Model.recentDownloads
    defaultLayout $ do
        setTitle "Bitlove: New Torrents"
        addFilterScript
        let links = [("Downloads", [("RSS", NewRssR, BC.unpack typeRss),
                                    ("ATOM", NewAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        [whamlet|
<section class="col">
  <h2>New Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
  ^{renderPagination page}
|]

getTopR :: Handler RepHtml
getTopR = do
    page <- makePage
    downloads <- withDB $
                 withPage page $
                 Model.popularDownloads
    defaultLayout $ do
        setTitle "Bitlove: Popular Torrents"
        addFilterScript
        let links = [("Downloads", [("RSS", TopRssR, BC.unpack typeRss),
                                    ("ATOM", TopAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        [whamlet|
<section class="col">
  <h2>Popular Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
  ^{renderPagination page}
|]

getTopDownloadedR :: Period -> Handler RepHtml
getTopDownloadedR period = do
  let (period_days, period_title) = 
        case period of
          PeriodDays 1 -> (1, "1 day")
          PeriodDays days -> (days, show days ++ " days")
          PeriodAll -> (10000, "all time")
  page <- makePage
  downloads <- withDB $
               withPage page $
               Model.mostDownloaded period_days
  lift $ lift $ putStrLn $ "render " ++ (show $ length downloads) ++ " downloads"
  defaultLayout $ do
    setTitle "Bitlove: Top Downloaded"
    addFilterScript
    let links = [("Downloads", [("RSS", TopDownloadedRssR period, BC.unpack typeRss),
                                ("ATOM", TopDownloadedAtomR period, BC.unpack typeAtom)
                               ])]
    addFeedsLinks links
    [whamlet|
<section class="col">
  <h2>Top downloaded in #{period_title}
  ^{renderFeedsList links}
  ^{renderDownloads downloads True}
  ^{renderPagination page}
|]

getUserR :: UserName -> Handler RepHtml
getUserR user = do
  canEdit' <- canEdit user
  page <- makePage
  let fetch =
          withDB $ \db -> do
                  detailss <- Model.userDetailsByName user db
                  case detailss of
                    [] ->
                        return Nothing
                    (details:_) -> 
                        do feeds <- Model.userFeeds user canEdit' db
                           downloads <- withPage page (Model.userDownloads user) db
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
                  [whamlet|
<header class="user">
  <div class="meta">
    $if not (T.null $ userImage details)
        <img class="logo"
             src=@{UserThumbnailR user (Thumbnail 64)}>
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
           src="@{UserFeedThumbnailR user (feedSlug feed) (Thumbnail 64)}">
      <div>
        <h3>
          <a href="@{UserFeedR user (feedSlug feed)}">#{feedTitle feed}
        $if not (T.null $ feedHomepage feed)
          <p class="homepage">
            <a rel="me"
               href="#{feedHomepage feed}">#{feedHomepage feed}
        $if not (feedPublic feed)
          <p .note>Private

<section class="col2">
  <h2>Recent Torrents
  ^{renderFeedsList links}
  ^{renderDownloads downloads False}
  ^{renderPagination page}
      |]
  
  fetch >>= maybe notFound render
  

getUserFeedR :: UserName -> Text -> Handler RepHtml
getUserFeedR user slug = do
  canEdit' <- canEdit user
  page <- makePage
  mFeedDownloadsErrors <-
      withDB $ \db -> do
        feeds <- Model.userFeedInfo user slug db
        case feeds of
          [] ->
              return Nothing
          (feed:_) ->
              do downloads <-
                     withPage page (Model.feedDownloads $ feedUrl feed) db
                 enclosureErrors <-
                     if canEdit'
                     then Model.enclosureErrors (feedUrl feed) db
                     else return []
                 return $ Just (feed, downloads, enclosureErrors)

  case mFeedDownloadsErrors of          
    Nothing ->
        notFound
    Just (feed, downloads, enclosureErrors) ->
        do let links = [("Subscribe",
                         [("Feed", MapFeedR user slug, BC.unpack typeRss)]),
                        ("Just Downloads", 
                         [("RSS", UserFeedRssR user slug, BC.unpack typeRss),
                          ("ATOM", UserFeedAtomR user slug, BC.unpack typeAtom)
                         ])]
               mError 
                   | not canEdit' = Nothing
                   | otherwise = case feedError feed of
                                   Just "" -> Nothing
                                   mE -> mE
           defaultLayout $
                      do setTitle $ toMarkup $ feedTitle feed `T.append` " on Bitlove"
                         when canEdit' $
                              addScript $ StaticR $ StaticRoute ["edit-feed.js"] []
                         addFeedsLinks links
                         [whamlet|
<section class="col">
  <header class="feed">
    <div class="meta">
      <img class="logo"
           src="@{UserFeedThumbnailR user slug (Thumbnail 64)}">
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
        $maybe error <- mError
          <div class="error">
             <h3>Feed Error
             <p><pre>#{error}
             <p .hint>Right now, the error message will not be cleared in case of HTTP 304 Not modified. Don't worry too much if your torrents appear to be fine.

  $if not (null enclosureErrors)
    <div .error>
       <h3>Enclosure Errors
       <dl .enclosureErrors>
         $forall enclosureError <- enclosureErrors
           <dt>#{fst enclosureError}
           <dd><pre>#{snd enclosureError}
  ^{renderFeedsList links}
  ^{renderDownloads downloads False}
  ^{renderPagination page}
    |]

renderDownloads downloads showOrigin =
    [hamlet|
$forall item <- Model.groupDownloads downloads
  ^{renderItem item showOrigin}
      |]

renderItem item showOrigin =
  let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing ++ "\n%H:%M") $
             itemPublished item
      isOnlyDownload = length (itemDownloads item) == 1
      bandwidth d
          | downloadDownspeed d < 100 * 1024 =
              Nothing
          | otherwise =
              Just $
              let (n, unit) = humanSize' $ fromIntegral $
                              downloadDownspeed d
                  n' :: String
                  n' | n < 10 = 
                         show (fromIntegral (truncate $ n * 10) / 10 :: Double)
                     | otherwise = 
                         show $ truncate n
              in (n', unit ++ "B/s")
      seeders = (+ 1) . downloadSeeders
      types = map downloadType $ itemDownloads item
      countType t = length $ filter (== t) types
      isOnlyType = (== 1) . countType
  in [hamlet|
  <article class="item"
           id="#{itemId item}"
           xml:lang="#{fromMaybe "" $ itemLang item}">
    <div>
      $if not (T.null $ itemImage item)
        <img class="logo"
             src="@{UserFeedItemThumbnailR (itemUser item) (itemSlug item) (itemId item) (Thumbnail 48)}">
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
        $else
           <p class="homepage">
             <a href="#{itemHomepage item}">#{itemHomepage item}
    $forall d <- itemDownloads item
      <ul class="download">
        <li class="torrent">
          <a href="@{TorrentFileR (downloadUser d) (downloadSlug d) (TorrentName $ downloadName d)}"
             title="Download #{downloadName d} with BitTorrent"
             rel="enclosure"
             data-type="#{downloadType d}">
            $if isOnlyType (downloadType d)
              <span>#{downloadLabel d}
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
            <dt>#{seeders d}
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
addFeedsLinks lists = do
  let addFeedsLink (title :: Text, route, type_) =
          [hamlet|
           <link rel="alternate"
                 type="#{type_}"
                 href="@{route}"
                 title="#{title}">
           |]
  addHamletHead [hamlet|
     $forall feed <- concat $ map snd lists
       ^{addFeedsLink feed}
                 |]

renderFeedsList lists = do
  renderRoute <-
    lift $
    isMiro >>= \isMiro' ->
    (liftIO $ putStrLn $ "isMiro=" ++ show isMiro') >>
    (if isMiro'
     then (("http://subscribe.getmiro.com/?type=video&url1=" `T.append`) <$>)
     else id) <$>
    getFullUrlRender
  let renderFeedsList' (title :: Text, feeds) =
              [hamlet|
               <dt>#{title}:
               $forall feed <- feeds
                 <dd>^{renderFeedsList'' feed}
               |]
      renderFeedsList'' (title :: Text, route, type_) =
              [hamlet|
               <a href="#{renderRoute route}"
                  type=#{type_}>#{title}
               |]
  [whamlet|
     <dl class="feedslist">
       $forall list <- lists
         ^{renderFeedsList' list}
     |]

pageParameter :: Handler Int
pageParameter = (clamp . fromInt 1 . T.unpack . fromMaybe "") <$> 
                lookupGetParam "page"
    where fromInt d s =
              case reads s of
                [(i, "")] -> i
                _ -> d
          clamp = min maxPages .
                  max 1
                
maxPages = 10
                
renderPagination page =
    [hamlet|
     <nav .pagination>
       $maybe previous <- pagePrevious page
         <p .previous>
           <a href="#{previous}">⟸
       $maybe next <- pageNext page
         <p .next>
           <a href="#{next}">⟹
     |]

makePage :: Handler Page
makePage = do 
  p <- pageParameter
  url <- getUrlRender
  mRoute <- getCurrentRoute
  let pageLink p' =
          ((`T.append`
            ("?page=" `T.append`
             T.pack (show p'))) . url) <$> 
          mRoute
      clamp n
            | n < 1 || n > maxPages = 
                Nothing
            | otherwise =
                Just n
  return $
         Page { pageNumber = p
              , pagePrevious = clamp (p - 1) >>= pageLink
              , pageNext = clamp (p + 1) >>= pageLink
              }
                                    
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

downloadLabel :: Download -> Text
downloadLabel d =
    fromMaybe (fromMaybe (downloadName d) $
               T.toUpper <$> ext (downloadName d)) $
    downloadType d `lookup` byType
      
    where byType = [ ("audio/mpeg", "MP3")
                   , ("audio/ogg", "OGG")
                   , ("video/mp4", "MP4")
                   , ("audio/x-m4a", "M4A")
                   , ("application/ogg", "OGG")
                   , ("video/x-m4v", "M4V")
                   , ("audio/mp3", "MP3")
                   , ("video/quicktime", "MOV")
                   , ("audio/mp4", "M4A")
                   , ("audio/x-mp3", "MP3")
                   , ("audio/m4a", "M4A")
                   , ("video/webm", "WebM")
                   , ("application/pdf", "PDF")
                   , ("video/ogg", "OGV")
                   , ("application/x-bittorrent", "Torrent")
                   , ("video/x-mp4", "MP4")
                   , ("video/x-flv", "FLV")
                   , ("text/html", "HTML")
                   , ("audio-mp4a-latm", "M4A")
                   ]
          ext fn = 
              let fn' = T.takeWhile (not . (`elem` "?#")) fn
                  mayLast :: [a] -> Maybe a
                  mayLast xs 
                      | null xs = Nothing
                      | otherwise = Just $
                                    xs !! (length xs - 1)
              in mayLast (T.splitOn "/" fn') >>=
                 (mayLast . T.splitOn ".")
                 