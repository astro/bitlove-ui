module WorkQueue where

import Prelude
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

    
newtype Queue = Queue (Chan (ResourceT IO ()))
    
makeQueue :: IO Queue
makeQueue = do
  chan <- newChan
  let handleE :: E.SomeException -> IO ()
      handleE = print
      loop = 
          do f <- readChan chan
             E.catch (runResourceT f) handleE
             loop
              
  _ <- forkIO loop
  return $ Queue chan

enqueue :: Queue -> ResourceT IO () -> IO ()
enqueue (Queue chan) =
    writeChan chan
