module Handler.FeedLookupAPI where

import Prelude
import Control.Monad
import qualified Data.Text as T
import Yesod

import Import

getFeedLookupJson :: Handler RepJson
getFeedLookupJson = do
  -- For CORS:
  addHeader "Access-Control-Allow-Origin" "*"

  urls <- map snd .
          filter (("url" `T.isPrefixOf`) . fst) .
          reqGetParams <$>
          getRequest
  fullUrlRender <- getFullUrlRender
  RepJson . toContent . object <$>
          withDB
          (\db ->
               forM urls $ \url ->
                   (url .=) .
                   map (\feed ->
                            fullUrlRender $
                            MapFeedR (feedUser feed) (feedSlug feed)
                   ) <$>
                   feedByUrl url db
          )
