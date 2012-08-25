module Handler.Help where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Import

getHelpR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpNavigation
    $(whamletFile "templates/help.hamlet")

getHelpPodcasterR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpNavigation
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-podcaster.hamlet")

getHelpFeedsR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpNavigation
    renderHelpPodcasterNavigation
    $(whamletFile "templates/help-feeds.hamlet")

getHelpApiR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpNavigation
    renderHelpPodcasterNavigation
    let example = T.pack $
                  BC.unpack $(embedFile "templates/help-api-example.text")
    $(whamletFile "templates/help-api.hamlet")

getHelpWidgetR =
  defaultLayout $ do
    setTitleI MsgTitleHelp
    renderHelpNavigation
    renderHelpPodcasterNavigation
    let example = T.pack $
                  BC.unpack $(embedFile "templates/help-widget-example.text")
    $(whamletFile "templates/help-widget.hamlet")

renderHelpNavigation =
    toWidget [hamlet|
              <h2>Help
              <div class="navtabs">
                <ul>
                  <li>
                    <a href="@{HelpR}">Users</a>
                  <li>
                    <a href="@{HelpPodcasterR}">Podcasters</a>
              |]

renderHelpPodcasterNavigation =
    toWidget [hamlet|
              <h2>Help for Podcasters</h2>
              <div class="navtabs">
                <ul>
                  <li>
                    <a href="@{HelpPodcasterR}">Getting started</a>
                  <li>
                    <a href="@{HelpFeedsR}">Feeds</a>
                  <li>
                    <a href="@{HelpWidgetR}">Widgets</a>
                  <li>
                    <a href="@{HelpApiR}">API</a>
              |]
