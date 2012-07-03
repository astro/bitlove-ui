module BitloveAuth where

import Prelude ((.), return, (>>=))
import Yesod
import Data.Maybe

import Model.User

login :: UserName -> GHandler y y' ()
login = setSession "user" . userName

sessionUser :: GHandler y y' (Maybe UserName)
sessionUser = lookupSession "user" >>=
              return . maybe Nothing (Just . UserName)
      
logout :: GHandler y y' ()
logout = deleteSession "user"
