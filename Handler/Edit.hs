module Handler.Edit where

import Prelude
import Import
import Data.Maybe
import Data.Text (Text, isPrefixOf)
import Control.Monad

import qualified Model as Model
import BitloveAuth

getUserDetailsR :: UserName -> Handler RepJson
getUserDetailsR user = do
  detailss <- withDB $ Model.userDetailsByName user
  case detailss of
    [] ->
      notFound
    details:_ ->
      returnJson [ "title" .= userTitle details
                 , "image" .= userImage details
                 , "homepage" .= userHomepage details
                 ]

postUserDetailsR :: UserName -> Handler RepJson
postUserDetailsR user = do
  title <- fromMaybe (userName user) `fmap` 
           lookupPostParam "title"
  image <- fromMaybe "" `fmap` 
           lookupPostParam "image"
  homepage <- fromMaybe "" `fmap` 
              lookupPostParam "homepage"
  let details = Model.UserDetails title image homepage
  withDB $
         Model.setUserDetails user details
  returnJson ([] :: [(Text, Int)])

putUserFeedR :: UserName -> Text -> Handler RepJson
putUserFeedR user slug = do
  mUrl <- lookupPostParam "url"
  -- Validate
  url <- case mUrl of
           Just url
               | "http://" `isPrefixOf` url ||
                 "https://" `isPrefixOf` url ->
                     return url
           Nothing ->
               sendResponse $ RepJson $ toContent $ 
               object ["error" .= ("Invalid URL" :: Text)]
      
  isNew <- withDB $ Model.addUserFeed user slug url
  
  link <- ($ UserFeedR user slug) `fmap`
          getUrlRender
  returnJson ["link" .= link]

deleteUserFeedR :: UserName -> Text -> Handler RepJson
deleteUserFeedR user slug = do
  withDB $ Model.deleteUserFeed user slug
  link <- ($ UserR user) `fmap`
          getUrlRender
  returnJson ["link" .= link]

getUserFeedDetailsR :: UserName -> Text -> Handler RepJson
getUserFeedDetailsR user slug = do
  detailss <- withDB $ Model.userFeedDetails user slug
  case detailss of
    [] ->
      notFound
    details:_ ->
      returnJson [ "public" .= fdPublic details
                 , "title" .= fdTitle details
                 ]

postUserFeedDetailsR :: UserName -> Text -> Handler RepJson
postUserFeedDetailsR user slug = do
  public <- maybe False (== "true") `fmap`
            lookupPostParam "public"
  title <- lookupPostParam "title"
  let details = FeedDetails public title
  withDB $ Model.setUserFeedDetails user slug details
  returnJson ([] :: [(Text, Int)])

deleteTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepJson
deleteTorrentFileR user slug (TorrentName name) = do
  withDB $ Model.purgeTorrent user slug name
  returnJson ([] :: [(Text, Int)])
  
returnJson = return . RepJson . toContent . object 
