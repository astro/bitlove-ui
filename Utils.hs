module Utils (iso8601, toHex, fromHex) where

import Prelude
import Data.Time
import System.Locale
import qualified Data.ByteString as B
import qualified Data.Text as T
import Numeric (readHex, showHex)


iso8601 :: LocalTime -> String
iso8601 time =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") time ++
    zone
    where zone = case formatTime defaultTimeLocale "%z" time of
                   (sig:h1:h2:m1:m2) ->
                     sig:h1:h2:':':m1:m2
                   _ ->
                     "Z"

toHex :: B.ByteString -> T.Text
toHex = T.pack . concatMap mapByte . B.unpack
    where mapByte = pad 2 '0' . flip showHex ""
          pad len padding s
              | length s < len = pad len padding $ padding:s
              | otherwise = s

fromHex :: T.Text -> B.ByteString
fromHex = B.pack . hexToWords
  where hexToWords text
          | T.null text = []
          | otherwise = 
            let (hex, text') = T.splitAt 2 text
                w = fst $ head $ readHex $ T.unpack hex
            in w:(hexToWords text')
