module Handler.Impress where

import Import

getImpressR :: HandlerT UIApp IO Html
getImpressR =
  defaultLayout $ do
    setTitleI MsgImpressTitle
    $(whamletFile "templates/impress.hamlet")
