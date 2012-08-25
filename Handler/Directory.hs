module Handler.Directory where

import Data.List
import qualified Data.ByteString.Char8 as BC

import PathPieces
import qualified Model
import Import
import Handler.Browse (renderFeedsList, addFeedsLinks)


getDirectoryR :: Handler RepHtml
getDirectoryR = do
  dir <- groupDirectory `fmap` withDB (Model.getDirectory)
  let (dir1, dir2) = splitAt ((length dir + 1) `div` 2) dir
  defaultLayout $ do
    setTitleI MsgTitleDirectory
    let links = [("Feeds", [("OPML", DirectoryOpmlR, BC.unpack typeOpml)])]
    addFeedsLinks links
    [whamlet|
              <h2>_{MsgHeadingDirectory}
              ^{renderFeedsList links}
              <section class="col1 directory">
                ^{renderEntries dir1}
              <section class="col2 directory">
                ^{renderEntries dir2}
              |]
    where renderEntries entries =
              [hamlet|
               $forall es <- entries
                 <article class="meta">
                   <img class="logo"
                        src="@{UserThumbnailR (Model.dirUser $ head es) (Thumbnail 64)}">
                   <div class="title">
                     <h3>
                       <a href="@{UserR $ Model.dirUser $ head es}">#{Model.dirUserTitle $ head es}
                   <ul class="feeds">
                     $forall e <- es
                       <li xml:lang="#{Model.dirFeedLang e}"
                           data-types="#{Model.dirFeedTypes e}">
                         <a href="@{UserFeedR (Model.dirUser e) (Model.dirFeedSlug e)}">
                           #{Model.dirFeedTitle e}
               |]

typeOpml :: ContentType
typeOpml = "text/x-opml"

newtype RepOpml = RepOpml Content

instance HasReps RepOpml where
    chooseRep (RepOpml content) _cts =
        return (typeOpml, content)


getDirectoryOpmlR :: Handler RepOpml
getDirectoryOpmlR = do
  dir <- groupDirectory `fmap` withDB (Model.getDirectory)
  url <- getFullUrlRender
  RepOpml `fmap`
         hamletToContent [xhamlet|
<opml version="2.0">
  <head title="Bitlove.org directory"
        ownerId="#{url DirectoryR}">
  <body>
    $forall es <- dir
      <outline text="#{Model.dirUserTitle $ head es}"
               htmlUrl="#{url $ UserR (Model.dirUser $ head es)}">
        $forall e <- es
          <outline text="#{Model.dirFeedTitle e}"
                   type="rss"
                   htmlUrl="#{url $ UserFeedR (Model.dirUser e) (Model.dirFeedSlug e)}"
                   xmlUrl="#{url $ MapFeedR (Model.dirUser e) (Model.dirFeedSlug e)}">
                          |]


groupDirectory :: [Model.DirectoryEntry] -> [[Model.DirectoryEntry]]
groupDirectory = groupBy $
                 \e1 e2 ->
                 Model.dirUser e1 == Model.dirUser e2