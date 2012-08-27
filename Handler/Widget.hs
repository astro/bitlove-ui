module Handler.Widget where

import Prelude
import Data.FileEmbed (embedFile)
import Blaze.ByteString.Builder (fromByteString)
import Blaze.ByteString.Builder.Internal.Types (Builder)
import Data.Monoid

import Import

newtype RepJs = RepJs Content

instance HasReps RepJs where
    chooseRep (RepJs content) _ = return (typeJavascript, content)

serveJs :: Monad m => Builder -> m RepJs
serveJs = return . RepJs . flip ContentBuilder Nothing

wrapJs :: Builder -> Builder
wrapJs js = mconcat
            [ fromByteString "(function() { "
            , js
            , fromByteString " })();"
            ]

getWidgetBaseR :: Handler RepJs
getWidgetBaseR =
    serveJs $
    fromByteString "window.torrentByEnclosure = " `mappend`
    wrapJs (fromByteString $(embedFile "templates/widget-base.js"))
    
getWidgetPowerpressR :: Handler RepJs
getWidgetPowerpressR =
    serveJs $
    wrapJs $
    fromByteString $(embedFile "templates/widget-base.js")
    `mappend`
    fromByteString $(embedFile "templates/widget-powerpress.js")

getWidgetPodpressR :: Handler RepJs
getWidgetPodpressR =
    serveJs $
    wrapJs $
    fromByteString $(embedFile "templates/widget-base.js")
    `mappend`
    fromByteString $(embedFile "templates/widget-podpress.js")
