module Tracker.WebsocketHub where

import Prelude
import Data.Int (Int16)
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Data.Convertible
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Control.Concurrent.STM
import qualified Database.PostgreSQL.LibPQ as PQ

import Model.Query (query')
import Model.Download (InfoHash)
import Model.Tracker (PeerId)

import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Resource

type SessionId = Int16 -- ^(Reuses peer port)
type SessionsRef = TVar (HM.HashMap (PeerId, SessionId) (TChan Text))

data Hub = Hub { hubSessionsRef :: SessionsRef
               }

notificationChannel :: String
notificationChannel = "websocket"

startHub :: PQ.Connection -> IO Hub
startHub db = do
  void $
    query' ("LISTEN " ++ notificationChannel ++ ";") [] db

  sessions <- newTVarIO HM.empty
  let hubLoop = do
        mn <- PQ.notifies db
        case mn of
          Nothing ->
            threadDelay 100000
          Just n
            | BC.unpack (PQ.notifyRelname n) == notificationChannel -> do
                let msg = decodeUtf8 $ PQ.notifyExtra n
                putStrLn $ "TODO: msg " ++ show msg

  void $ forkIO $ forever hubLoop
  putStrLn "Started Websocket Hub…"
  return $ Hub { hubSessionsRef = sessions
               }

hubSend :: PQ.Connection -> Text -> IO ()
hubSend db msg =
  void $
  query' ("NOTIFY " ++ notificationChannel ++ ", ?;")
  [convert msg] db

data Websession = Websession { sessionId :: SessionId
                             , sessionsRef :: SessionsRef
                             , sessionChan :: TChan Text
                             }

newSession :: Hub -> SessionId -> IO Websession
newSession hub sessionId = do
  chan <- newTChanIO
  return $
    Websession { sessionId = sessionId
               , sessionsRef = hubSessionsRef hub
               , sessionChan = chan
               }

sessionRegisterFor :: Websession -> PeerId -> IO ()
sessionRegisterFor session peerId =
  atomically $
  modifyTVar' (sessionsRef session) $
  HM.insert (peerId, sessionId session) $
  sessionChan session

sessionRecv :: Websession -> IO (Maybe Text)
sessionRecv =
  atomically .
  tryReadTChan . sessionChan

sessionClear :: Websession -> IO ()
sessionClear session =
  atomically $
  modifyTVar' (sessionsRef session) $
  HM.filterWithKey $ \(_peerId, sessionId') _ ->
  sessionId' == sessionId session
