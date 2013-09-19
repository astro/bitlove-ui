module Handler.Help where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Import

getHelpR :: HandlerT UIApp IO Html
getHelpR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    $(whamletFile "templates/help.hamlet")

getHelpPodcasterR :: HandlerT UIApp IO Html
getHelpPodcasterR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-podcaster.hamlet")

getHelpFeedsR :: HandlerT UIApp IO Html
getHelpFeedsR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-feeds.hamlet")

getHelpApiR :: HandlerT UIApp IO Html
getHelpApiR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    let by_enclosure_example = 
            T.pack $
            BC.unpack $(embedFile "templates/help-api-by-enclosure-example.text")
        feed_lookup_example = 
            T.pack $
            BC.unpack $(embedFile "templates/help-api-feed-lookup-example.text")
    $(whamletFile "templates/help-api.hamlet")

getHelpWidgetR :: HandlerT UIApp IO Html
getHelpWidgetR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    let example = T.pack $
                  BC.unpack $(embedFile "templates/help-widget-example.text")
    $(whamletFile "templates/help-widget.hamlet")

renderHelpPodcasterNavigation :: WidgetT UIApp IO ()
renderHelpPodcasterNavigation =
    toWidget [hamlet|$newline always
              <ul #podcaster-help-nav>
                <li>
                  <a href="@{HelpPodcasterR}">Getting started</a>
                <li>
                  <a href="@{HelpFeedsR}">Feeds</a>
                <li>
                  <a href="@{HelpWidgetR}">Widgets</a>
                <li>
                  <a href="@{HelpApiR}">API</a>
              |]
