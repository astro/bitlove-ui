module Tracker.Handler.ExportStream where

import Prelude
import Yesod
import Yesod.Default.Config (appExtra)
import qualified Data.HashSet as HashSet
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Blaze.ByteString.Builder (Builder, fromByteString)

import Foundation (extraExportAuth)
import Tracked
import Tracker.Foundation
import Tracker.Utils

-- | Returns (isAuthenticated, username)
checkAuth :: Handler (Bool, Text)
checkAuth = do
  mAuth <- lookupBasicAuth
  case mAuth of
    Nothing ->
      return (False, "")
    Just (user, password) -> do
      let auth = T.concat [user, ":", password]
      isAuthenticated <- elem auth .
                         extraExportAuth .
                         appExtra . settings <$>
                         getYesod
      return (isAuthenticated, user)

getExportStream :: Handler RepPlain
getExportStream = do
  (isAuthenticated, user) <- checkAuth
  case isAuthenticated of
    True -> do
      addr <- getRemoteAddr
      liftIO $ putStrLn $ "ExportStream for " ++ show user ++ " connected from " ++ show addr

      RepPlain <$> ContentSource <$>
        exportStream <$>
        trackerTracked <$> getYesod
  
    False ->
      notAuthenticated


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
      BC.concat
      [ "# Hello\r\n"
      , "# \r\n"
      , "# This service shall 0-rate our content with Internet Service Providers.\r\n"
      , "# \r\n"
      , "# Persistent storage of the data obtained via this method is prohibited.\r\n"
      , "# \r\n"
      ]
