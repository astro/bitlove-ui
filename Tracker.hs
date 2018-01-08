{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker where

import Prelude
import Yesod

import Foundation (DBPool)
import Cache
import Tracker.Foundation
import Tracker.Handler.HTTP
import Tracker.Handler.Webtorrent
import Tracker.Handler.ExportStream
import Tracked

mkYesodDispatch "TrackerApp" resourcesTrackerApp

makeTrackerApp :: DBPool -> Tracked -> IO TrackerApp
makeTrackerApp pool tracked =
    do cache <- newCache "localhost" 11211
       return $ TrackerApp pool cache tracked
