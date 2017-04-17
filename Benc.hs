module Benc where

import Prelude
import Data.List (sort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Blaze.ByteString.Builder
import Data.String


data BValue = BInt Integer
            | BString LBC.ByteString
            | BList [BValue]
            | BDict [(BValue, BValue)]
              deriving (Show, Eq, Ord)
                       
instance IsString BValue where
    fromString = BString . LBC.pack

-- FIXME: Builder inserts newlines between each chunk?
  
toBuilder :: BValue -> Builder
toBuilder (BString s) = 
    mconcat [ fromByteString $ BC.pack $ show $ LBC.length s
            , fromByteString ":"
            , fromLazyByteString s
            ]
toBuilder (BInt i) = 
    mconcat [ fromByteString "i"
            , fromByteString $ BC.pack $ show i
            , fromByteString "e"
            ]
toBuilder (BList xs) = 
    mconcat [ fromByteString "l"
            , mconcat $ map toBuilder xs
            , fromByteString "e"
            ]
toBuilder (BDict xs) =
    let xs' = sort xs
    in mconcat [ fromByteString "d"
               , mconcat $
                 map (\(k, v) ->
                          toBuilder k `mappend`
                          toBuilder v
                     ) xs'
               , fromByteString "e"
               ]
