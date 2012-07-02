{-# LANGUAGE OverloadedStrings #-}
module Handler.TorrentStats where

import Yesod
import Data.Time
import System.Locale
import qualified Data.Text as T

import Import
import PathPieces
import qualified Model as Model


getTorrentStatsR :: UserName -> Text -> Text -> StatsPeriod -> StatsJSON -> Handler RepJson
getTorrentStatsR user slug name statsPeriod stats = do
  info_hashes <- withDB $
                 Model.infoHashByName user slug name
  case info_hashes of
    [] -> notFound
    (info_hash:_) -> do
      let (period, interval) = 
              case statsPeriod of
                StatsDay -> (24 * 60 * 60, 60 * 60)
                StatsWeek -> (7 * 24 * 60 * 60, 6 * 60 * 60)
                StatsMonth -> (30 * 24 * 60 * 60, 24 * 60 * 60)
                StatsYear -> (365 * 24 * 60 * 60, 7 * 24 * 60 * 60)
      stop <- lift $ lift $ getCurrentTime
      timezone <- lift $ lift $ getCurrentTimeZone
      let start = (-period) `addUTCTime` stop
          stop' = utcToLocalTime timezone stop
          start' = utcToLocalTime timezone start
          baseJson = ["start" .= iso8601 start',
                      "stop" .= iso8601 stop',
                      "interval" .= interval]
  
      case stats of
        -- TODO
        StatsDownloads -> do
          downloads <- withDB $
                       Model.get_counter "complete" info_hash start' stop' interval
          return $ RepJson $ toContent $
            object $ ("downloads" .= statsToJson downloads) : baseJson


statsToJson :: [StatsValue] -> Value
statsToJson = object .
              map (\(StatsValue time val) ->
                    (T.pack $ iso8601 time) .= val
                  )
