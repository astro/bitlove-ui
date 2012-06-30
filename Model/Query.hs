{-# LANGUAGE RankNTypes #-}
module Model.Query where

import Prelude
import Database.HDBC
import Data.Convertible
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC
import Numeric (readHex, showOct, readOct)
import Data.Char (chr, ord)


type Query e = IConnection conn => conn -> IO [e]

query :: (IConnection conn,
          Convertible [SqlValue] e
         ) => String -> [SqlValue] -> conn -> IO [e]
query sql args conn = do
  rows <- quickQuery' conn sql args
  return $ do Right val <- safeConvert `fmap` rows
              return val

fromBytea :: SqlValue -> ByteString
fromBytea = unescape . fromSql
  where unescape :: Text -> ByteString
        unescape text =
          case T.splitAt 2 text of
            ("\\x", hex) ->
              pack $ hexToWords hex
            _ ->
              pack $ octToWords text
        hexToWords text
          | T.null text = []
          | otherwise = let (hex, text') = T.splitAt 2 text
                            w = fst $ head $ readHex $ T.unpack hex
                        in w:(hexToWords text')
        octToWords = go . T.unpack
          where go ('\\':t) = let (oct, t') = splitAt 3 t
                              in (fst $ head $ readOct oct):(go t')
                go (c:t) = (fromIntegral $ ord c):(go t)
                go [] = []
                           
toBytea :: ByteString -> SqlValue
toBytea = toSql . BC.pack . concatMap (escape . fromIntegral) . unpack
  where escape 92 = "\\\\"
        escape c | c >= 32 && c <= 126 = [chr c]
        escape c = "\\" ++ oct c
        oct c = pad 3 '0' $ showOct c ""
        pad :: Int -> Char -> String -> String
        pad targetLen padding s
          | length s >= targetLen = s
          | otherwise = pad targetLen padding (padding:s)
