module Tracker.Handler.ExportStream where

import qualified Data.HashSet as HashSet
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import Blaze.ByteString.Builder (Builder, fromByteString)

import Prelude
import Yesod
import Tracked
import Tracker.Foundation
import Tracker.Utils

getExportStream :: Handler RepPlain
getExportStream = do
  addr <- getRemoteAddr
  liftIO $ putStrLn $ "ExportStream connected from " ++ show addr

  RepPlain <$> ContentSource <$>
    exportStream <$>
    trackerTracked <$> getYesod
  

exportStream :: Tracked -> Source (ResourceT IO) (Flush Builder)
exportStream tracked = do
  yield $ Chunk $ fromByteString motd
  yield Flush
  stream HashSet.empty

  where
    stream known = do
      liftIO $ putStrLn $ "getChangedAddrs with " ++ show (HashSet.size known) ++ " known addrs"
      (known', removed, added) <- liftIO $ getChangedAddrs tracked known

      forM_ removed $ \addr ->
        yield $ Chunk $ fromByteString $
        BC.concat ["- ", BC.pack $ show addr, "\r\n"]
      forM_ added $ \addr ->
        yield $ Chunk $ fromByteString $
        BC.concat ["+ ", BC.pack $ show addr, "\r\n"]
      yield Flush

      -- Loop:
      stream known'

    motd =
      "# Hello\r\n"
