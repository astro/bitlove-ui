module Handler.Help where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Import

getHelpR :: GHandler sub UIApp RepHtml
getHelpR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    $(whamletFile "templates/help.hamlet")

getHelpPodcasterR :: GHandler sub UIApp RepHtml
getHelpPodcasterR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-podcaster.hamlet")

getHelpFeedsR :: GHandler sub UIApp RepHtml
getHelpFeedsR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-feeds.hamlet")

getHelpApiR :: GHandler sub UIApp RepHtml
getHelpApiR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    let example = T.pack $
                  BC.unpack $(embedFile "templates/help-api-example.text")
    $(whamletFile "templates/help-api.hamlet")

getHelpWidgetR :: GHandler sub UIApp RepHtml
getHelpWidgetR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpPodcasterNavigation
    let example = T.pack $
                  BC.unpack $(embedFile "templates/help-widget-example.text")
    $(whamletFile "templates/help-widget.hamlet")

renderHelpPodcasterNavigation :: GWidget sub UIApp ()
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
