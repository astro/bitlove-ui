{-# LANGUAGE RankNTypes #-}
module Model.Query where

import Prelude
import Database.HDBC
import Data.Convertible
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC
import Numeric (showOct, readOct)
import Data.Char (chr, ord)
import Debug.Trace

import Utils


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
          trace ("unescape " ++ show text) $
          case T.splitAt 2 text of
            ("\\x", hex) ->
              fromHex hex
            _ ->
              pack $ octToWords text
        octToWords = go . T.unpack
          where go ('\\':c:t)
                  | c `elem` "\\\"\'\n\r\t" = fromIntegral (ord c) : go t
                go ('\\':t) = let (oct, t') = splitAt 3 t
                              in case readOct oct of
                                [(n, "")] -> 
                                  n : go t'
                                _ -> 
                                  error $ "Cannot read oct " ++ show oct
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
