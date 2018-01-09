{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Tracker where

import Prelude
import Yesod
import Yesod.Default.Config

import Foundation (DBPool, BitloveEnv, Extra)
import Cache
import Tracker.Foundation
import Tracker.Handler.HTTP
import Tracker.Handler.Webtorrent
import Tracker.Handler.ExportStream
import Tracked

mkYesodDispatch "TrackerApp" resourcesTrackerApp

makeTrackerApp :: AppConfig BitloveEnv Extra -> DBPool -> Tracked -> IO TrackerApp
makeTrackerApp conf pool tracked =
    do cache <- newCache "localhost" 11211
       return $ TrackerApp conf pool cache tracked
