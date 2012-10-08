module Handler.Widget where

import Prelude
import Data.FileEmbed (embedFile)
import Blaze.ByteString.Builder (fromByteString)
import Blaze.ByteString.Builder.Internal.Types (Builder)
import Data.Monoid

import Import


cacheWidget :: GHandler a b ()
cacheWidget = cacheSeconds $ 60 * 60

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
    do cacheWidget
       serveJs $
               fromByteString "window.torrentByEnclosure = "
               `mappend`
               wrapJs (fromByteString $(embedFile "templates/widget-base.js")
                       `mappend`
                       fromByteString "return resolve;"
                      )
    
getWidgetPowerpressR :: Handler RepJs
getWidgetPowerpressR =
    do cacheWidget
       serveJs $
               wrapJs $
               fromByteString $(embedFile "templates/widget-base.js") 
               `mappend`
               fromByteString $(embedFile "templates/widget-powerpress.js")

getWidgetPodpressR :: Handler RepJs
getWidgetPodpressR =
    do cacheWidget
       serveJs $
               wrapJs $
               fromByteString $(embedFile "templates/widget-base.js")
               `mappend`
               fromByteString $(embedFile "templates/widget-podpress.js")
