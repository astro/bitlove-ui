{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker where

import Prelude
import Yesod
import Data.Pool (takeResource)

import qualified WorkQueue as WQ
import Foundation (DBPool)
import Cache
import Tracker.Foundation
import Tracker.Handler.HTTP
import Tracker.Handler.Webtorrent
import Tracker.WebsocketHub

mkYesodDispatch "TrackerApp" resourcesTrackerApp

makeTrackerApp :: DBPool -> IO TrackerApp
makeTrackerApp pool =
    do cache <- newCache "localhost" 11211
       aq <- WQ.makeQueue
       sq <- WQ.makeQueue
       (db, _localPool) <- takeResource pool
       hub <- startHub db
       return $ TrackerApp pool cache aq sq hub
