{-# LANGUAGE RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Query where

import Prelude
import Control.Monad
import qualified Database.PostgreSQL.LibPQ as PQ
import Data.Convertible
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Default

import Model.SqlValue

txBegin :: PQ.Connection -> IO ()
txBegin db = do
  Just _ <- PQ.exec db "BEGIN"
  return ()

txCommit :: PQ.Connection -> IO ()
txCommit db = do
  Just _ <- PQ.exec db "COMMIT;"
  return ()

txRollback :: PQ.Connection -> IO ()
txRollback db = do
  result <- PQ.exec db "ROLLBACK;"
  case result of
    Just _ -> return ()
    Nothing -> void $ PQ.reset db
  return ()

type Query e = PQ.Connection -> IO [e]

replacePlaceholders :: String -> String
replacePlaceholders = go (1 :: Int)
  where go n ('?':s) = "$" ++ show n ++ (go (n + 1) s)
        go n (c:s) = c : go n s
        go _ "" = ""

query' :: String -> [SqlValue] -> PQ.Connection -> IO (Maybe [[SqlValue]])
query' sql args conn = do
  let sql' = BC.pack $ replacePlaceholders sql
      args' = map (\v ->
                     case v of
                       SqlValue oid bytes ->
                         Just $
                         (oid, bytes, PQ.Binary)
                       SqlNull ->
                         Nothing
                  ) args

  putStrLn $ "Query: " ++ sql ++ " " ++ show args
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
          putStrLn $ "colOids: " ++ show colOids

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

query1 :: (Convertible SqlValue e)
      => String -> [SqlValue] -> PQ.Connection -> IO [e]
query1 sql args conn =
  maybe [] (map (convert . head)) <$>
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
  
