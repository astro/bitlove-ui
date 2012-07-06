module Handler.Edit where

import Prelude
import Import
import Data.Maybe
import Data.Text (Text)

import qualified Model as Model

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
putUserFeedR user slug = undefined
