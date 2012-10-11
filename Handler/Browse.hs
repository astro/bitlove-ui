{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Handler.Browse where

import qualified Data.Text as T
import Data.Maybe
import Data.Foldable (Foldable)
import Data.Time.Format
import System.Locale
import Network.HTTP.Types (parseQueryText)
import Text.Blaze
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html5 hiding (div, details, map)
import Text.Blaze.Html5.Attributes hiding (item, min, max, id)
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Settings.StaticFiles

import PathPieces
import qualified Model
import Import
import BitloveAuth

data Page = Page { pageNumber :: Int
                 , pagePrevious :: Maybe Text
                 , pageNext :: Maybe Text
                 }

pageSize :: Int
pageSize = 20

withPage :: Page -> (Model.QueryPage -> a) -> a
withPage page f =
    f $ Model.QueryPage (pageSize * 2) $ (pageNumber page - 1) * pageSize

makePage :: Handler Page
makePage = do 
  pageParam <- pageParameter
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
         Page { pageNumber = pageParam
              , pagePrevious = clamp (pageParam - 1) >>= pageLink
              , pageNext = clamp (pageParam + 1) >>= pageLink
              }

paginate :: (Model.QueryPage -> Transaction [a]) -> Handler (Page, [a])
paginate f = do
  page <- makePage
  xs <- withDB $ withPage page $ f
  return $ updatePagination page xs
  
updatePagination :: Page -> [a] -> (Page, [a])
updatePagination page xs
    | length xs <= pageSize =
        (page { pageNext = Nothing }, 
         xs)
    | otherwise =
        (page, 
         take pageSize xs)

getFrontR :: Handler RepHtml
getFrontR = do
  downloads <- withDB $
               Model.mostDownloaded 1 (Model.QueryPage 20 0)
  defaultLayout $ do
    setTitleI MsgTitle
    $(whamletFile "templates/front.hamlet")
    [whamlet|$newline always
     <section class="downloads">
       <ul>
         $forall item <- Model.groupDownloads downloads
           <li>
             <a href="@{UserFeedR (itemUser item) (itemSlug item)}##{itemId item}">
               <img src="@{UserFeedItemThumbnailR (itemUser item) (itemSlug item) (itemId item) (Thumbnail 64)}">
     |]

getNewR :: Handler RepHtml
getNewR = do
    (page, downloads) <- paginate Model.recentDownloads
    defaultLayout $ do
        setTitleI MsgTitleNew
        addFilterScript
        let 
            links :: [(Text, [(Text, Route UIApp, String)])]
            links = [("Downloads", [("RSS", NewRssR, BC.unpack typeRss),
                                    ("ATOM", NewAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        [whamlet|$newline always
         <section class="col">
           <h2>_{MsgNewTorrents}
           ^{renderFeedsList links}
           ^{renderDownloads downloads True}
           ^{renderPagination page}
         |]

getTopR :: Handler RepHtml
getTopR = do
    (page, downloads) <- paginate
                         Model.popularDownloads
    defaultLayout $ do
        setTitleI MsgTitleTop
        addFilterScript
        let 
            links :: [(Text, [(Text, Route UIApp, String)])]
            links = [("Downloads", [("RSS", TopRssR, BC.unpack typeRss),
                                    ("ATOM", TopAtomR, BC.unpack typeAtom)
                                   ])]
        addFeedsLinks links
        [whamlet|$newline always
         <section class="col">
           <h2>_{MsgTopTorrents}
           ^{renderFeedsList links}
           ^{renderDownloads downloads True}
           ^{renderPagination page}
         |]

getTopDownloadedR :: Period -> Handler RepHtml
getTopDownloadedR period = do
  let period_days =
        case period of
          PeriodDays 1 -> 1
          PeriodDays days -> days
          PeriodAll -> 10000
  (page, downloads) <- paginate $
                       Model.mostDownloaded period_days
  lift $ lift $ putStrLn $ "render " ++ (show $ length downloads) ++ " downloads"
  defaultLayout $ do
    setTitleI MsgTitleTopDownloaded
    addFilterScript
    let links = [("Downloads", [("RSS", TopDownloadedRssR period, BC.unpack typeRss),
                                ("ATOM", TopDownloadedAtomR period, BC.unpack typeAtom)
                               ])]
    addFeedsLinks links
    [whamlet|$newline always
     <section class="col">
       <h2>
         $case period
           $of PeriodDays n
             $if n == 1
               \ _{MsgTopDownloadedDay}
             $else
               \ _{MsgTopDownloadedDays n}
           $of PeriodAll
             \ _{MsgTopDownloadedAll}
       ^{renderFeedsList links}
       ^{renderDownloads downloads True}
       ^{renderPagination page}
     |]

getUserR :: UserName -> Handler RepHtml
getUserR user = do
  canEdit' <- canEdit user
  let fetch = do
          page <- makePage
          withDB $ \db -> do
                  detailss <- Model.userDetailsByName user db
                  case detailss of
                    [] ->
                        return Nothing
                    (details:_) -> 
                        do feeds <- Model.userFeeds user canEdit' db
                           downloads <- withPage page (Model.userDownloads user) db
                           let (page', downloads') = updatePagination page downloads
                           return $ Just (page', details, feeds, downloads')
      render (page, details, feeds, downloads) =
          defaultLayout $ do 
                  setTitleI $ MsgTitleUser $ userName user
                  when canEdit' $
                       addScript $ StaticR js_edit_user_js
                  let links = [("Downloads", [("RSS", UserDownloadsRssR user, BC.unpack typeRss),
                                              ("ATOM", UserDownloadsAtomR user, BC.unpack typeAtom)
                                             ])]
                  addFeedsLinks links
                  [whamlet|$newline always
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
                             <p .note>_{MsgPrivate}
                   
                   <section class="col2">
                     <h2>Recent Torrents
                     ^{renderFeedsList links}
                     ^{renderDownloads (take pageSize downloads) False}
                     ^{renderPagination page}
                   |]
  
  fetch >>= maybe notFound render
  

getUserFeedR :: UserName -> Text -> Handler RepHtml
getUserFeedR user slug = do
  canEdit' <- canEdit user
  mPageFeedDownloadsErrors <- do
      page <- makePage
      withDB $ \db -> do
        feeds <- Model.userFeedInfo user slug db
        case feeds of
          [] ->
              return Nothing
          (feed:_) ->
              do downloads <-
                     withPage page (Model.feedDownloads $ feedUrl feed) db
                 let (page', downloads') = updatePagination page downloads
                 enclosureErrs <-
                     if canEdit'
                     then Model.enclosureErrors (feedUrl feed) db
                     else return []
                 return $ Just (page', feed, downloads', enclosureErrs)

  case mPageFeedDownloadsErrors of          
    Nothing ->
        notFound
    Just (page, feed, downloads, enclosureErrs) ->
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
                      do setTitleI $ MsgTitleFeed $ feedTitle feed
                         when canEdit' $
                              addScript $ StaticR js_edit_feed_js
                         addFeedsLinks links
                         [whamlet|$newline always
                          <section class="col">
                            <header class="feed">
                              <div class="meta">
                                <img class="logo"
                                     src="@{UserFeedThumbnailR user slug (Thumbnail 64)}">
                                <div class="title">
                                  <div>
                                    <h2>#{feedTitle feed}
                                    <p class="publisher">
                                      \ _{MsgBy} #
                                      <a href="@{UserR user}">#{userName user}
                                  $if canEdit'
                                    <p .note>#{feedUrl feed}
                                  $if not (T.null $ feedHomepage feed)
                                    <p class="homepage">
                                      <a rel="me"
                                         href="#{feedHomepage feed}">#{feedHomepage feed}
                                  $if not (feedPublic feed)
                                    <p class="hint">_{MsgPrivateExplain}
                                  $maybe err <- mError
                                    <div class="error">
                                       <h3>Feed Error
                                       <p><pre>#{err}
                                       <p .hint>_{MsgErrorExplain}
                          
                            $if not (null enclosureErrs)
                              <div .error>
                                 <h3>_{MsgEnclosureErrors}
                                 <dl .enclosureErrors>
                                   $forall enclosureError <- enclosureErrs
                                     <dt>#{fst enclosureError}
                                     <dd><pre>#{snd enclosureError}
                            ^{renderFeedsList links}
                            ^{renderDownloads (take pageSize downloads) False}
                            ^{renderPagination page}
                          |]

getSearchRedirectR :: Handler RepHtml
getSearchRedirectR = 
    lookupGetParam "q" >>=
    maybe notFound (redirect . SearchR)

getSearchR :: Text -> Handler RepHtml
getSearchR needle = do
        (page, feeds, downloads) <- do
          page <- makePage
          withDB $ \db -> do
            feeds <- Model.searchFeeds needle db
            downloads <- withPage page (Model.searchDownloads needle) db
            let (page', downloads') = updatePagination page downloads
            return (page', feeds, downloads')
        defaultLayout $ do 
          setTitleI $ MsgTitleSearch needle
          [whamlet|$newline always
           <h1>_{MsgSearchHeading needle}
           |]
          let renderFeeds =
                  [whamlet|
                   <h2>Feeds
                   $forall feed <- feeds
                     <article class="feed">
                       <img class="logo"
                            src="@{UserFeedThumbnailR (feedUser feed) (feedSlug feed) (Thumbnail 64)}">
                       <div>
                         <h3>
                           <a href="@{UserFeedR (feedUser feed) (feedSlug feed)}">#{feedTitle feed}
                         <p .feed>
                           \ _{MsgBy} #
                           <a href="@{UserR (feedUser feed)}">#{userName $ feedUser feed}
                         $if not (T.null $ feedHomepage feed)
                           <p class="homepage">
                             <a rel="me"
                                href="#{feedHomepage feed}">#{feedHomepage feed}
                   |]
              renderTorrents =
                  [whamlet|
                   <h2>Torrents
                   ^{renderDownloads downloads True}
                   ^{renderPagination page}
                   |]
          case (feeds, downloads) of
            ([], []) ->
                [whamlet|$newline always
                 <section .col>
                   <h2>_{MsgNothingFound}
                 |]
            (_:_, _:_) ->
                [whamlet|$newline always
                 <section .col1>
                   ^{renderFeeds}
                 <section .col2>
                   ^{renderTorrents}
                 |]
            ([], _) ->
                [whamlet|$newline always
                 <section .col>
                   ^{renderTorrents}
                 |]
            (_, []) ->
                [whamlet|$newline always
                 <section .col>
                   ^{renderFeeds}
                 |]

renderDownloads :: forall sub. [Download] -> Bool -> GWidget sub UIApp () 
renderDownloads downloads showOrigin =
    [whamlet|$newline always
     $forall item <- Model.groupDownloads downloads
       ^{renderItem item showOrigin}
     |]

renderItem :: forall sub. Item -> Bool -> GWidget sub UIApp ()
renderItem item showOrigin = do
  let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing ++ "\n%H:%M") $
             itemPublished item
      --isOnlyDownload = length (itemDownloads item) == 1
      stats :: Text -> Text -> Integer -> t -> Markup
      stats c t n = [hamlet|$newline always
                     <dl class=#{c}>
                       <dt>
                         #{n}
                       <dd>
                         $if n == 1
                           #{t}
                         $else
                           #{t}s
                   |]
      bandwidth d
          | downloadDownspeed d < 100 * 1024 =
              Nothing
          | otherwise =
              Just $
              let (n, unit) = humanSize' $ fromIntegral $
                              downloadDownspeed d
                  n' :: String
                  n' | n < 10 = 
                         show (fromIntegral (truncate $ n * 10 :: Integer) / 10 :: Double)
                     | otherwise = 
                         show $ (truncate n :: Integer)
              in (n', unit ++ "B/s")
      seeders = (+ 1) . downloadSeeders
      types = map downloadType $ itemDownloads item
      countType t = length $ filter (== t) types
      isOnlyType = (== 1) . countType
  [whamlet|$newline always
  <article class="item"
           id="#{itemId item}"
           xml:lang="#{fromMaybe "" $ itemLang item}">
    <div>
      $if not (T.null $ itemImage item)
        <img class="logo"
             src="@{UserFeedItemThumbnailR (itemUser item) (itemSlug item) (itemId item) (Thumbnail 48)}">
      <div class="right">
        $if not (T.null $ itemPayment item)
          <div class="flattr">
            #{renderPayment}
        <p class="published">#{date}
      <div class="title">
        <h3>
          <a href="@{UserFeedR (itemUser item) (itemSlug item)}##{itemId item}">#{itemTitle item}
        $if showOrigin
          <p class="feed">
            \ _{MsgIn} #
            <a href="@{UserFeedR (itemUser item) (itemSlug item)}">#{fromMaybe (itemSlug item) $ itemFeedTitle item}
            \ _{MsgBy} #
            <a href="@{UserR $ itemUser item}">#{userName $ itemUser item}
        $else
           <p class="homepage">
             <a href="#{itemHomepage item}">#{itemHomepage item}
    $forall d <- itemDownloads item
      <ul class="download">
        <li class="torrent">
          <a href="@{TorrentFileR (downloadUser d) (downloadSlug d) (TorrentName $ downloadName d)}"
             title="_{MsgDownloadTorrent $ downloadName d}"
             rel="enclosure"
             data-type="#{downloadType d}">
            $if isOnlyType (downloadType d)
              #{downloadLabel d} Torrent
            $else
              #{downloadName d}
            <span class="size" title="_{MsgDownloadSize}">
              #{humanSize (downloadSize d)}
        <li class="stats">
          $maybe (speed, unit) <- bandwidth d
              <dl>
                <dt>
                  #{speed}
                  <span class="unit"> #{unit}
                <dd>Download speed
                  
          ^{stats "seeders" "Seeder" $ seeders d}
          ^{stats "leechers" "Leecher" $ downloadLeechers d}
          ^{stats "downloads" "Download" $ downloadDownloaded d}
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

addFilterScript :: forall sub. GWidget sub UIApp ()
addFilterScript =
    addScript $ StaticR js_filter_js

-- | <link rel="alternate"> to <head>
addFeedsLinks :: forall sub master a a1.
                 ToMarkup a =>
                 [(a1, [(Text, Route master, a)])] -> GWidget sub master ()
addFeedsLinks lists = do
  let addFeedsLink (linkTitle :: Text, route, linkType) =
          [hamlet|$newline always
           <link rel="alternate"
                 type="#{linkType}"
                 href="@{route}"
                 title="#{linkTitle}">
           |]
  toWidgetHead [hamlet|$newline always
     $forall feed <- concat $ map snd lists
       ^{addFeedsLink feed}
                 |]

renderFeedsList :: forall a (t :: * -> *) sub (t1 :: * -> *).
                   (Foldable t1, Foldable t, ToMarkup a) =>
                   t1 (Text, t (Text, Route UIApp, a)) -> GWidget sub UIApp ()
renderFeedsList lists = do
  renderRoute <-
    lift $
    isMiro >>= \isMiro' ->
    (if isMiro'
     then (("http://subscribe.getmiro.com/?type=video&url1=" `T.append`) <$>)
     else id) <$>
    getFullUrlRender
  let renderFeedsList' (fTitle :: Text, feeds) =
              [hamlet|$newline always
               <dt>#{fTitle}:
               $forall feed <- feeds
                 <dd>^{renderFeedsList'' feed}
               |]
      renderFeedsList'' (fTitle :: Text, route, fType) =
              [hamlet|$newline always
               <a href="#{renderRoute route}"
                  type=#{fType}>
                 #{fTitle}
               |]
  [whamlet|$newline always
     <dl class="feedslist">
       $forall fList <- lists
         ^{renderFeedsList' fList}
     |]

pageParameter :: Handler Int
pageParameter = (clamp . fromInt 1 . T.unpack . fromMaybe "") <$> 
                lookupGetParam "page"
    where fromInt d s =
              case reads s of
                [(j, "")] -> j
                _ -> d
          clamp = min maxPages .
                  max 1

maxPages :: Int
maxPages = 10


renderPagination :: forall t. Page -> t -> MarkupM ()
renderPagination page =
    [hamlet|$newline always
     <nav .pagination>
       $maybe previous <- pagePrevious page
         <p .previous>
           <a href="#{previous}"><i class="icon-arrow-left"></i> Previous
       $maybe next <- pageNext page
         <p .next>
           <a href="#{next}">Next <i class="icon-arrow-right"></i>
     |]
                                    
humanSize :: (Integral a, Show a) => a -> String
humanSize n = let (n', unit) = humanSize' $ fromIntegral n
                  ns | n' < 10 = show $
                                 (fromIntegral (truncate $ n' * 10 :: Integer) / 10 :: Double)
                     | otherwise = show $ (truncate n' :: Integer)
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
                 