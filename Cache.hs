module Cache where

import Prelude
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Network.Socket (HostName, PortNumber)
import qualified Database.Memcache.Client as Memcache

import Model.Download (InfoHash(unInfoHash))

type Cache = Memcache.Client

newCache :: HostName -> PortNumber -> IO Cache
newCache host port = do
  let specs = [Memcache.ServerSpec host port Memcache.NoAuth]
      opts = Memcache.def
  Memcache.newClient specs opts

getTorrentExists :: InfoHash -> Cache -> IO Bool
getTorrentExists infoHash client = do
  let key = BC.concat ["E", unInfoHash infoHash]
      expire = 3600
  mVFV <- Memcache.gat client key expire
  case mVFV of
    Nothing -> return True
    Just (value, flags, version) -> return False

-- | Caching negative entries to avoid DB traffic on rogue torretns
setTorrentExists :: InfoHash -> Bool -> Cache -> IO ()
setTorrentExists infoHash exists client = do
  let key = BC.concat ["E", unInfoHash infoHash]
      value = "0"
      expire = 3600
  when (not exists) $
    void $
    Memcache.set client key value 0 expire
