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
      return $ RepJson $ toContent $
      object [ "title" .= userTitle details
             , "image" .= userImage details
             , "homepage" .= userHomepage details
             ]

postUserDetailsR :: UserName -> Handler RepJson
postUserDetailsR user = do
  canEdit <- canEdit user
  when (not canEdit) $
       permissionDenied "Not your user details"
  
  title <- fromMaybe (userName user) `fmap` 
           lookupPostParam "title"
  image <- fromMaybe "" `fmap` 
           lookupPostParam "image"
  homepage <- fromMaybe "" `fmap` 
              lookupPostParam "homepage"
  let details = Model.UserDetails title image homepage
  withDB $
         Model.setUserDetails user details
  return $ RepJson $ toContent $ object ([] :: [(Text, Int)])

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
  return $ RepJson $ toContent $
         object ["link" .= link]

deleteUserFeedR :: UserName -> Text -> Handler RepJson
deleteUserFeedR = undefined

getUserFeedDetailsR :: UserName -> Text -> Handler RepJson
getUserFeedDetailsR user slug = do
  detailss <- withDB $ Model.userFeedDetails user slug
  case detailss of
    [] ->
      notFound
    details:_ ->
      return $ RepJson $ toContent $
             object [ "public" .= fdPublic details
                    , "title" .= fdTitle details
                    ]

postUserFeedDetailsR :: UserName -> Text -> Handler RepJson
postUserFeedDetailsR = undefined
