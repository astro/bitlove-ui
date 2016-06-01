module Handler.Edit where

import Prelude
import Import hiding (returnJson)
import Data.Maybe
import qualified Data.Text as T
import Data.Char
import Control.Monad (when)

import qualified Model as Model

getUserDetailsR :: UserName -> Handler RepJson
getUserDetailsR user = do
  detailss <- withDB $ Model.userDetailsByName user
  case detailss of
    [] ->
      notFound
    details:_ ->
      returnJson ([ ("title", userTitle details)
                  , ("image", userImage details)
                  , ("homepage", userHomepage details)
                  ])

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
  when (not $ validateSlug slug) $
    sendResponse $ RepJson $ toContent $ 
    object ["error" .= ("Invalid slug" :: Text)]
  url <- case mUrl of
           Just url
               | "http://" `T.isPrefixOf` url ||
                 "https://" `T.isPrefixOf` url ->
                     return url
           _ ->
               sendResponse $ RepJson $ toContent $ 
               object ["error" .= ("Invalid URL" :: Text)]
      
  _isNew <- withDB $ Model.addUserFeed user slug url
  
  link <- ($ UserFeedR user slug) `fmap`
          getUrlRender
  returnJson [("link", link)]
    
    where validateSlug :: Text -> Bool
          validateSlug slg = T.length slg >= 1 &&
                              all (\c ->
                                    isAsciiLower c ||
                                    isDigit c ||
                                    c `elem` ("-_"::String)
                                  ) (T.unpack slg)

deleteUserFeedR :: UserName -> Text -> Handler RepJson
deleteUserFeedR user slug = do
  _deleted <- withDB $ Model.deleteUserFeed user slug
  link <- ($ UserR user) `fmap`
          getUrlRender
  returnJson [("link", link)]

getUserFeedDetailsR :: UserName -> Text -> Handler RepJson
getUserFeedDetailsR user slug = do
  detailss <- withDB $ Model.userFeedDetails user slug
  case detailss of
    [] ->
      notFound
    details:_ ->
      returnJson [ ("public", Just $ T.pack $ show $ fdPublic details)
                 , ("title", fdTitle details)
                 ]

postUserFeedDetailsR :: UserName -> Text -> Handler RepJson
postUserFeedDetailsR user slug = do
  public <- maybe False (== "true") `fmap`
            lookupPostParam "public"
  title <- lookupPostParam "title"
  let details = FeedDetails public title
  _ <- withDB $ Model.setUserFeedDetails user slug details
  returnJson ([] :: [(Text, Int)])

deleteTorrentFileR :: UserName -> Text -> TorrentName -> Handler RepJson
deleteTorrentFileR user slug (TorrentName name) = do
  _ <- withDB $ Model.purgeTorrent user slug name
  returnJson ([] :: [(Text, Int)])
  
returnJson :: ToJSON v => [(Text, v)] -> Handler RepJson
returnJson = return . repJson . object . 
             map (\(k, v) ->
                   (k, toJSON v)
                 )
