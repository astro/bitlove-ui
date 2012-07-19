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
      stop <- liftIO getCurrentTime
      tz <- liftIO getCurrentTimeZone
      let start = (-period) `addUTCTime` stop
          zonedStop = utcToZonedTime tz stop
          zonedStart = utcToZonedTime tz start
          localStart = utcToLocalTime tz start
          localStop = utcToLocalTime tz stop
          baseJson = ["start" .= iso8601 zonedStart,
                      "stop" .= iso8601 zonedStop,
                      "interval" .= interval]
          withStats :: (((Model.InfoHash -> LocalTime -> LocalTime -> 
                          Integer -> PostgreSQL.Connection
                          -> IO b) 
                         -> IO b)
                        -> IO a)
                     -> Handler a
          withStats f = withDB $ \db ->
                        let q f' = f' info_hash 
                                   localStart localStop
                                   interval db
                        in f q
  
      (RepJson . toContent . object) `fmap` case stats of
        StatsDownloads -> do
          downloads <- withStats ($ Model.getCounter "complete")
          return $ ("downloads" .= statsToJson tz downloads) : baseJson
        StatsTraffic -> do
          (down, up, up_seeder) <- withStats $ \q ->
            do down <- q $ Model.getCounter "down"
               up <- q $ Model.getCounter "up"
               up_seeder <- q $ Model.getCounter "up_seeder"
               return (down, up, up_seeder)
          return $ ("down" .= statsToJson tz down) :
                   ("up" .= statsToJson tz up) :
                   ("up_seeder" .= statsToJson tz up_seeder) : baseJson
        StatsSwarm -> do
          (seeders, leechers) <- withStats $ \q ->
            do seeders <- q $ Model.getGauge "seeders"
               leechers <- q $ Model.getGauge "leechers"
               let completeGauge vs =
                       completeGauge' $
                       case vs of
                         -- First beyond start?
                         (StatsValue t value):_
                             | t > localStart && 
                               value > 0 ->
                                 let t' = utcToLocalTime tz $
                                          (fromIntegral $ -interval) `addUTCTime` (localTimeToUTC tz t)
                                 in (StatsValue t' 0):vs
                         _ ->
                           vs
                   -- empty gauge
                   completeGauge' [] = []
                   -- last before stop?
                   completeGauge' [v@(StatsValue t value)]
                       | t < localStop = [v, StatsValue localStop value]
                   -- traverse till last (above case)
                   completeGauge' (v:vs) = v:(completeGauge' vs)
               return (completeGauge $ map cheat seeders,
                       completeGauge $ leechers)
          return $ ("seeders" .= statsToJson tz seeders) :
                   ("leechers" .= statsToJson tz leechers) : baseJson

  -- Our seeder is actually not included in stats
  where cheat :: StatsValue -> StatsValue
        cheat (StatsValue t v) = StatsValue t $ v + 1

statsToJson :: TimeZone -> [StatsValue] -> Value
statsToJson tz = 
    object .
    map (\(StatsValue time val) ->
             (T.pack $ iso8601 $ localTimeToZonedTime tz time) .= val
        )
