module Handler.Directory where

import Data.List
import qualified Data.ByteString.Char8 as BC

import qualified Model
import Import
import Handler.Browse (safeLogo, renderFeedsList)


getDirectoryR :: Handler RepHtml
getDirectoryR = do
  dir <- groupDirectory `fmap` withDB (Model.getDirectory)
  let (dir1, dir2) = splitAt ((length dir + 1) `div` 2) dir
  defaultLayout $ do
    setTitle "Bitlove: Directory"
    addHamletHead [hamlet|
                   <link rel="alternate" 
                         type="#{BC.unpack typeOpml}" 
                         href="@{DirectoryOpmlR}">
                   |]
    toWidget [hamlet|
              <h2>Directory of Torrentified Podcasters
              ^{feedsList}
              <section class="col1">
                ^{renderEntries dir1}
              <section class="col2">
                ^{renderEntries dir2}
              |]
    where feedsList = renderFeedsList [("Feeds:", [("OPML", DirectoryOpmlR)])]
          renderEntries entries =
              [hamlet|
               $forall es <- entries
                 <article class="meta">
                   <img class="logo"
                        src="#{safeLogo $ Model.dirUserImage $ head es}">
                   <div class="title">
                     <h2>
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
  RepOpml `fmap`
         hamletToContent [xhamlet|
<opml version="2.0">
  <head title="Bitlove.org directory"
        ownerId="http://bitlove.org@{DirectoryR}">
  <body>
    $forall es <- dir
      <outline text="#{Model.dirUserTitle $ head es}"
               htmlUrl="http://bitlove.org@{UserR (Model.dirUser $ head es)}">
        $forall e <- es
          <outline text="#{Model.dirFeedTitle e}"
                   type="rss"
                   htmlUrl="http://bitlove.org@{UserFeedR (Model.dirUser e) (Model.dirFeedSlug e)}"
                   xmlUrl="http://bitlove.org@{MapFeedR (Model.dirUser e) (Model.dirFeedSlug e)}">
                          |]


groupDirectory :: [Model.DirectoryEntry] -> [[Model.DirectoryEntry]]
groupDirectory = groupBy $
                 \e1 e2 ->
                 Model.dirUser e1 == Model.dirUser e2