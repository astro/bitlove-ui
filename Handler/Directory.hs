module Handler.Directory where

import Data.List
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower, isAlpha)
import qualified Data.Text as T

import PathPieces
import qualified Model
import Import
import Handler.Browse (renderFeedsList, addFeedsLinks)


getDirectoryR :: Handler RepHtml
getDirectoryR = do
  pages <- groupUsers `fmap` withDB Model.getActiveUsers
  liftIO $ putStrLn $ "pages: " ++ show (map fst pages)
  defaultLayout $ do
    setTitleI MsgTitleDirectory
    addFilterScript
    let 
        links :: [(Text, [(Text, Route UIApp, String)])]
        links = [("Feeds", [("OPML", DirectoryOpmlR, BC.unpack typeOpml)])]
    addFeedsLinks links
    [whamlet|$newline always
              <h2>_{MsgHeadingDirectory}
              ^{renderFeedsList links}
              <section .directory>
                $forall page <- pages
                  ^{renderLetter page}
              |]
    where renderLetter (letter, users) =
              [hamlet|$newline always
               <article .directory-page .filterable>
                 <p .letter>
                   <a href="@{DirectoryPageR letter}">
                     #{show letter}
                 <ul .users>
                   $forall u <- users
                     <li .filterable
                         data-langs="#{Model.activeFeedLangs u}"
                         data-types="#{Model.activeFeedTypes u}">
                       <a href="@{UserR $ Model.activeUser u}">
                         #{T.unpack $ Model.userName $ Model.activeUser u}
                       <span .feedcount>
                         (#{Model.activeFeeds u})
               |]
  

groupUsers :: [(Model.ActiveUser)] -> [(DirectoryPage, [Model.ActiveUser])]
groupUsers [] = []
groupUsers (u:us) = 
  let getLetter u' = 
        case T.unpack $ userName $ activeUser u' of
          c:_ | isAlpha c -> 
            DirectoryLetter $ toLower c
          _ -> 
            DirectoryDigit
      letter = getLetter u
      (us', us'') = break ((letter /=) . getLetter) us
  in (letter, (u : us')) : groupUsers us''

getDirectoryPageR :: DirectoryPage -> Handler RepHtml
getDirectoryPageR page = do
  dir <- groupDirectory `fmap` withDB (Model.getDirectory $ Just page)
  defaultLayout $ do
    setTitleI MsgTitleDirectory
    addFilterScript
    [whamlet|$newline always
              <h2>_{MsgHeadingDirectory}: #{show page}
              <p .before-filter>
                <a href=@{DirectoryR}>_{MsgDirectoryIndexBack}
              <section class="directory">
                $forall es <- dir
                  ^{renderEntry es}
              |]
    where renderEntry es =
              [hamlet|$newline always
               <article .meta .filterable>
                 <img .logo
                      src="@{UserThumbnailR (Model.dirUser $ head es) (Thumbnail 64)}">
                 <div .title>
                   <h3>
                     <a href="@{UserR $ Model.dirUser $ head es}">#{Model.dirUserTitle $ head es}
                 <ul .feeds>
                   $forall e <- es
                     <li .filterable
                         xml:lang="#{Model.dirFeedLang e}"
                         data-types="#{Model.dirFeedTypes e}">
                       <a href="@{UserFeedR (Model.dirUser e) (Model.dirFeedSlug e)}">
                         #{Model.dirFeedTitle e}
               |]

groupDirectory :: [Model.DirectoryEntry] -> [[Model.DirectoryEntry]]
groupDirectory = groupBy $
                 \e1 e2 ->
                 Model.dirUser e1 == Model.dirUser e2

typeOpml :: ContentType
typeOpml = "text/x-opml"

newtype RepOpml = RepOpml Content
    deriving (ToContent)

instance ToTypedContent RepOpml where
    toTypedContent (RepOpml content) =
        TypedContent typeOpml content


getDirectoryOpmlR :: Handler RepOpml
getDirectoryOpmlR = do
  dir <- groupDirectory `fmap` withDB (Model.getDirectory Nothing)
  url <- getFullUrlRender
  return $ RepOpml $ toContent $
             [xhamlet|$newline always
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
                          |] url
                 