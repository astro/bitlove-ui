module BitloveAuth where

import Prelude ((.), return, (>>=), elem, Bool (False), maybe)
import Yesod
import Data.Maybe

import Model.User

login :: UserName -> GHandler y y' ()
login = setSession "user" . userName

sessionUser :: GHandler y y' (Maybe UserName)
sessionUser = lookupSession "user" >>=
              return . maybe Nothing (Just . UserName)
      
canEdit :: UserName -> GHandler sub master Bool
canEdit user = sessionUser >>=
               return . maybe False (`elem` [user, UserName "astro"])
  
logout :: GHandler y y' ()
logout = deleteSession "user"
