{-# LANGUAGE RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Query where

import Prelude
import Database.HDBC
import Data.Convertible
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Numeric (showOct, readOct)
import Data.Char (chr)
import qualified Control.Exception as E
import System.IO
import Control.Applicative
import Data.Default

import Utils

instance Convertible [SqlValue] T.Text where
  safeConvert = safeConvert . head

type Query e = IConnection conn => conn -> IO [e]

query :: (IConnection conn,
          Convertible [SqlValue] e
         ) => String -> [SqlValue] -> conn -> IO [e]
query sql args conn = do
  rows <- quickQuery' conn sql args
  concat <$> mapM tryRow rows
    where tryRow row =
              do let caught :: E.SomeException -> String
                     caught = show
                 leftRight <- E.catch (return $
                                       either (Left . show) Right $
                                       safeConvert row)
                              (return . Left . caught)
                 case leftRight of
                   Right x -> 
                       return [x]
                   Left e ->
                       do hPutStrLn stderr $ "cannot safeConvert:\n" ++
                                    show row ++ "\n" ++ e
                          return []
                          
data QueryPage = QueryPage { pageLimit :: Int
                           , pageOffset :: Int
                           }
                 
instance Default QueryPage where
    def = QueryPage 25 0

instance Convertible QueryPage [SqlValue] where
    safeConvert (QueryPage limit offset) =
        do limit' <- safeConvert limit
           offset' <- safeConvert offset
           return [limit', offset']
  
-- TODO: return LB?
fromBytea :: SqlValue -> LBC.ByteString
fromBytea SqlNull = ""
fromBytea sql =
  let text :: String
      text = fromSql sql
  in 
    case text of
      '\\':'x':hex ->
          fromHex hex
      text' ->
          let unescape ('\\':c:t)
                  | c `elem` "\\\"\'" = 
                      c : unescape t
                  | c == 'n' =
                      '\n' : unescape t
                  | c == 'r' =
                      '\r' : unescape t
                  | c == 't' =
                      '\t' : unescape t
              unescape ('\\':t) = 
                  let (oct, t') = splitAt 3 t
                  in 
                    case readOct oct of
                      [(n, "")] -> 
                          chr n : unescape t'
                      _ -> 
                          error $ "Cannot read oct " ++ show oct
              unescape (c:t) = c : unescape t
              unescape "" = ""

        in LBC.pack $ unescape text'
                           
-- Strict variant of above
fromBytea' :: SqlValue -> B.ByteString
fromBytea' = B.concat . LBC.toChunks . fromBytea
           
-- TODO: BC.pack neccessary?
toBytea :: LB.ByteString -> SqlValue
toBytea = toSql . BC.pack . concatMap (escape . fromIntegral) . LB.unpack
  where escape 92 = "\\\\"
        escape c | c >= 32 && c <= 126 = [chr c]
        escape c = "\\" ++ oct c
        oct c = pad 3 '0' $ showOct c ""
        pad :: Int -> Char -> String -> String
        pad targetLen padding s
          | length s >= targetLen = s
          | otherwise = pad targetLen padding (padding:s)

toBytea' :: B.ByteString -> SqlValue
toBytea' = toBytea . LB.fromChunks . (:[])
