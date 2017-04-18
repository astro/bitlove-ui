{-# LANGUAGE RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Query where

import Prelude
import Control.Monad
import qualified Database.PostgreSQL.LibPQ as PQ
import Data.Convertible
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Numeric (showOct, readOct)
import Data.Char (chr)
import qualified Control.Exception as E
import System.IO
import Data.Default

import Utils
import Model.SqlValue


-- WTF?
instance Convertible [SqlValue] T.Text where
  safeConvert = safeConvert . head

type Query e = PQ.Connection -> IO [e]

query' :: String -> [SqlValue] -> PQ.Connection -> IO (Maybe [[SqlValue]])
query' sql args conn = do
  let sql' = BC.pack sql
      args' = map (\v ->
                     case v of
                       SqlValue oid bytes ->
                         Just $
                         (oid, bytes, PQ.Binary)
                       SqlNull ->
                         Nothing
                  ) args

  mResult <- PQ.execParams conn sql' args' PQ.Binary

  case mResult of
    Nothing -> return Nothing
    Just result -> do
      status <- PQ.resultStatus result
      case status of
        PQ.CommandOk ->
          return $ Just []
        _
          | status == PQ.SingleTuple ||
            status == PQ.TuplesOk -> do
          PQ.Row rows <- PQ.ntuples result
          PQ.Col cols <- PQ.nfields result

          colOids <-
            forM [0..(cols - 1)] $
            PQ.ftype result . PQ.toColumn

          fmap Just $
            forM [0..(rows - 1)] $ \row ->
            forM (zip [0..(cols - 1)] colOids) $ \(col, oid) ->
               SqlValue oid <$>
               fromMaybe B.empty <$>
               PQ.getvalue result (PQ.toRow row) (PQ.toColumn col)
        _ ->
          error $ "pq: " ++ show status

query :: (Convertible [SqlValue] e)
      => String -> [SqlValue] -> PQ.Connection -> IO [e]
query sql args conn =
  maybe [] (map convert) <$>
  query' sql args conn

                          
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
fromBytea = const undefined
-- fromBytea SqlNull = ""
-- fromBytea sql =
--   let text :: String
--       text = convert sql
--   in 
--     case text of
--       '\\':'x':hex ->
--           fromHex hex
--       text' ->
--           let unescape ('\\':c:t)
--                   | c `elem` ("\\\"\'"::String) =
--                       c : unescape t
--                   | c == 'n' =
--                       '\n' : unescape t
--                   | c == 'r' =
--                       '\r' : unescape t
--                   | c == 't' =
--                       '\t' : unescape t
--               unescape ('\\':t) = 
--                   let (oct, t') = splitAt 3 t
--                   in 
--                     case readOct oct of
--                       [(n, "")] -> 
--                           chr n : unescape t'
--                       _ -> 
--                           error $ "Cannot read oct " ++ show oct
--               unescape (c:t) = c : unescape t
--               unescape "" = ""

--         in LBC.pack $ unescape text'
                           
-- Strict variant of above
fromBytea' :: SqlValue -> B.ByteString
fromBytea' = B.concat . LBC.toChunks . fromBytea
           
-- TODO: BC.pack neccessary?
toBytea :: LB.ByteString -> SqlValue
toBytea = const undefined
-- toBytea = convert . BC.pack . concatMap (escape . fromIntegral) . LB.unpack
--   where escape 92 = "\\\\"
--         escape c | c >= 32 && c <= 126 = [chr c]
--         escape c = "\\" ++ oct c
--         oct c = pad 3 '0' $ showOct c ""
--         pad :: Int -> Char -> String -> String
--         pad targetLen padding s
--           | length s >= targetLen = s
--           | otherwise = pad targetLen padding (padding:s)

toBytea' :: B.ByteString -> SqlValue
toBytea' = toBytea . LB.fromChunks . (:[])
