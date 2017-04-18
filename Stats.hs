module Stats (statsMiddleware) where

import Prelude
import Control.Concurrent
import Control.Concurrent.STM
import qualified Network.Wai as Wai
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import Control.Monad.Trans.Resource (runResourceT)

import Foundation (DBPool, withDBPool, BitloveEnv)
import Model.Stats (addCounter)
import Model.Download (InfoHash (InfoHash))

type Key = (T.Text, BC.ByteString)
type StatsBuffer = TVar (Map Key (TVar Integer))

statsMiddleware :: BitloveEnv -> DBPool -> IO Wai.Middleware
statsMiddleware env pool = do
  tBuf <- newTVarIO Map.empty
  return $ \app req respond ->
      do liftIO $ countRequest env pool tBuf req
         app req respond
   
countRequest :: BitloveEnv -> DBPool -> StatsBuffer -> Wai.Request -> IO ()
countRequest env pool tBuf req
    | "/static/" `BC.isPrefixOf` Wai.rawPathInfo req =
        -- Ignore static resources
        return ()
    | Wai.rawPathInfo req == "/by-enclosure.json" =
        -- Track referrer for API calls
        let referrer = fromMaybe "" $ "Referer" `lookup` Wai.requestHeaders req
        in increaseCounter pool tBuf ("by-enclosure.json", referrer) 1
    | otherwise =
        -- All others: just method & path
        let kind = "ui/" `T.append` T.pack (show env)
            info = Wai.requestMethod req `BC.append`
                   " " `BC.append`
                   Wai.rawPathInfo req
        in increaseCounter pool tBuf (kind, info) 1
    
increaseCounter :: DBPool -> StatsBuffer -> Key -> Integer -> IO ()
increaseCounter pool tBuf key increment = do
  isNew <-
      atomically $ do
        buf <- readTVar tBuf
        case key `Map.lookup` buf of
          Nothing ->
              do tIncrement <- newTVar increment
                 writeTVar tBuf $ Map.insert key tIncrement buf
                 return True
          Just tIncrement ->
              do modifyTVar tIncrement (+ increment)
                 return False
                 
  when isNew $ do
    _ <- forkIO $ do
      threadDelay second
      flushCounter pool tBuf key
    return ()
      
second :: Int
second = 1000000

flushCounter :: DBPool -> StatsBuffer -> Key -> IO ()
flushCounter pool tBuf key = do
  increment <-
      atomically $ do
        buf <- readTVar tBuf
        increment <- maybe (return 0) readTVar $ 
                     key `Map.lookup` buf
        writeTVar tBuf $ Map.delete key buf
        return increment
          
  let (kind, info) = key
  case increment of
    increment'
        | increment' > 0 ->
            runResourceT $
            withDBPool pool $ 
            addCounter kind (InfoHash info) increment'
    _ ->
        putStrLn $ "Warning: stale counter for " ++ show key
