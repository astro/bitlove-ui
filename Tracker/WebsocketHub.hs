module Tracker.WebsocketHub where

import Prelude
import Data.Int (Int16)
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Data.Convertible
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Lazy as LHM
import qualified Database.PostgreSQL.LibPQ as PQ

import Model.Query (query')
import Model.Download (InfoHash)
import Model.Tracker (PeerId(PeerId))
import Tracker.Utils

import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Resource

type SessionId = Int16 -- ^(Reuses peer port)
type SessionsRef = TVar (HM.HashMap PeerId (SessionId, TChan Object))

data Hub = Hub { hubSessionsRef :: SessionsRef
               }

notificationChannel :: String
notificationChannel = "websocket"

startHub :: PQ.Connection -> IO Hub
startHub db = do
  void $
    query' ("LISTEN " ++ notificationChannel ++ ";") [] db

  sessions <- newTVarIO HM.empty
    :: IO SessionsRef
  let hubLoop = do
        mn <- PQ.notifies db
        putStrLn $ "Notifies: " ++ show mn
        case mn of
          Nothing ->
            threadDelay 1000000
          Just n
            | BC.unpack (PQ.notifyRelname n) == notificationChannel -> do
                let mMsg :: Maybe Object
                    mMsg = decode $ LBC.fromChunks [PQ.notifyExtra n]
                let mPeerId = PeerId <$>
                              encodeLatin1 <$>
                              (mMsg >>=
                               LHM.lookup "peer_id" >>=
                               parseMaybe parseJSON
                              )
                case (mMsg, mPeerId) of
                  (Just msg, Just peerId) ->
                    atomically $ do
                    mSession <-
                      HM.lookup peerId <$>
                      readTVar sessions
                    case mSession of
                      Just (_sessionId, chan) ->
                        writeTChan chan msg
                      Nothing ->
                        return ()
                  _ ->
                    return ()
            | otherwise ->
                return ()
                
  void $ forkIO $ forever hubLoop
  putStrLn "Started Websocket Hub…"
  return $ Hub { hubSessionsRef = sessions
               }

hubSend :: ToJSON a => PQ.Connection -> a -> IO ()
hubSend db msg =
  void $
  query' "SELECT pg_notify($1 :: TEXT, $2 :: TEXT);"
  [convert (T.pack notificationChannel), convert msg'] db
  where msg' = encode msg

data Websession = Websession { sessionId :: SessionId
                             , sessionsRef :: SessionsRef
                             , sessionChan :: TChan Object
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
  HM.insert peerId $
  (sessionId session, sessionChan session)

sessionRecv :: Websession -> IO (Maybe Object)
sessionRecv =
  atomically .
  tryReadTChan . sessionChan

sessionClear :: Websession -> IO ()
sessionClear session =
  atomically $
  modifyTVar' (sessionsRef session) $
  HM.filterWithKey $ \_peerId (sessionId', _) ->
  sessionId' == sessionId session
