module Benc where

import Prelude
import Data.Monoid
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

  
toBuilder :: BValue -> Builder
toBuilder (BString s) = 
    insertLazyByteString s
toBuilder (BInt i) = 
    mconcat [ insertByteString "i"
            , insertByteString $ BC.pack $ show i
            , insertByteString "e"
            ]
toBuilder (BList xs) = 
    mconcat [ insertByteString "l"
            , mconcat $ map toBuilder xs
            , insertByteString "e"
            ]
toBuilder (BDict xs) =
    let xs' = sort xs
    in mconcat [ insertByteString "d"
               , mconcat $
                 map (\(k, v) ->
                          toBuilder k `mappend`
                          toBuilder v
                     ) xs'
               , insertByteString "e"
               ]
