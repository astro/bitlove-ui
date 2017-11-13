{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker where

import Prelude
import Yesod

import qualified WorkQueue as WQ
import Foundation (DBPool)
import Cache
import Tracker.Foundation
import Tracker.Handler.HTTP
import Tracker.Handler.Webtorrent
import Tracked

mkYesodDispatch "TrackerApp" resourcesTrackerApp

makeTrackerApp :: DBPool -> IO TrackerApp
makeTrackerApp pool =
    do cache <- newCache "localhost" 11211
       aq <- WQ.makeQueue
       sq <- WQ.makeQueue
       tracked <- newTracked
       return $ TrackerApp pool cache aq sq tracked
