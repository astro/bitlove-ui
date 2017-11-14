module Benc (BValue(..), toBuilder, parseBenc) where

import Prelude hiding (take, takeWhile)
import Data.List (sort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Blaze.ByteString.Builder
import Data.String
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8


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

bencoding :: Parser BValue
bencoding =
    integer <|> string <|> list <|> dict
    where integer =
              do char 'i'
                 sign <- takeWhile (== '-')
                 digits <- takeWhile1 isDigit
                 char 'e'
                 return $
                   BInt $ read $
                   (BC.unpack sign) ++ (BC.unpack digits)
          string =
              do len <- read <$> BC.unpack <$> takeWhile1 isDigit
                 char ':'
                 BString <$>
                   LBC.fromChunks <$>
                   (: []) <$>
                   take len
          list =
              do char 'l'
                 BList <$> manyTill bencoding (char 'e')
          dict =
              do char 'd'
                 map <- manyTill (do
                                   k <- bencoding
                                   v <- bencoding
                                   return (k, v)
                                 ) (char 'e')
                 map `seq`
                     return $ BDict map

parseBenc :: BC.ByteString -> Maybe BValue
parseBenc =
  maybeResult .
  parse bencoding
