module Handler.Impress where

import Import

getImpressR :: HandlerFor UIApp Html
getImpressR =
  defaultLayout $ do
    setTitleI MsgImpressTitle
    $(whamletFile "templates/impress.hamlet")
