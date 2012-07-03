module BitloveAuth where

import Prelude ((.), return, (>>=))
import Yesod
import Data.Maybe

import Model.User

sessionUser :: GHandler y y' (Maybe UserName)
sessionUser = lookupSession "user" >>=
              return . maybe Nothing (Just . UserName)
      
logout :: GHandler y y' ()
logout = deleteSession "user"
