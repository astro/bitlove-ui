{-# LANGUAGE OverloadedStrings #-}
module Handler.TorrentStats where

import Yesod
import Data.Time
import qualified Data.Text as T
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)

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
          withStats :: (((Model.InfoHash -> LocalTime ->
                           LocalTime -> Integer -> PostgreSQL.Connection
                          -> IO b) 
                         -> IO b)
                        -> IO a)
                     -> Handler a
          withStats f = withDB $ \db ->
                        let q f' = f' info_hash start' stop' interval db
                        in f q
  
      (RepJson . toContent . object) `fmap` case stats of
        StatsDownloads -> do
          downloads <- withStats ($ Model.getCounter "complete")
          return $ ("downloads" .= statsToJson downloads) : baseJson
        StatsTraffic -> do
          (down, up, up_seeder) <- withStats $ \q ->
            do down <- q $ Model.getCounter "down"
               up <- q $ Model.getCounter "up"
               up_seeder <- q $ Model.getCounter "up_seeder"
               return (down, up, up_seeder)
          return $ ("down" .= statsToJson down) :
                   ("up" .= statsToJson up) :
                   ("up_seeder" .= statsToJson up_seeder) : baseJson
        StatsSwarm -> do
          (seeders, leechers) <- withStats $ \q ->
            do seeders <- q $ Model.getGauge "seeders"
               leechers <- q $ Model.getGauge "leechers"
               return (seeders, leechers)
          return $ ("seeders" .= statsToJson seeders) :
                   ("leechers" .= statsToJson leechers) : baseJson


statsToJson :: [StatsValue] -> Value
statsToJson = object .
              map (\(StatsValue time val) ->
                    (T.pack $ iso8601 time) .= val
                  )
