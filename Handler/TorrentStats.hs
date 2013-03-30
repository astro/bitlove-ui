{-# LANGUAGE OverloadedStrings #-}
module Handler.TorrentStats where

import Yesod
import Data.Time
import qualified Data.Text as T
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)

import Import
import PathPieces
import qualified Model as Model
import BitloveAuth


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
          withStats :: (LocalTime -> LocalTime -> Integer -> Transaction a)
                     -> Handler a
          withStats f = withDB $ 
                        f localStart localStop interval
          withStats' :: (((LocalTime -> LocalTime -> Integer -> Transaction a) -> IO a) -> IO b) -> Handler b
          withStats' f = withDB $ \db ->
                         f $ \f' ->
                         f' localStart localStop interval db
  
      (RepJson . toContent . object) <$> case stats of
        StatsDownloads -> do
          canEdit <- canEdit user
          json <- ("downloads" .=) <$> 
                  statsToJson tz <$>
                  withStats (Model.getCounter "complete" info_hash)
          json' <- 
            if canEdit
            then do 
              path <- ($ TorrentFileR user slug $ TorrentName name) <$>
                      getUrlRender
              stats <- withStats (Model.getDownloadCounter path)
              return ["torrent" .= statsToJson tz stats]
            else return []
          return $ json : json' ++ baseJson
        StatsTraffic -> do
          (down, up, up_seeder) <- withStats' $ \q ->
            do down <- q $ Model.getCounter "down" info_hash
               up <- q $ Model.getCounter "up" info_hash
               up_seeder <- q $ Model.getCounter "up_seeder" info_hash
               return (down, up, up_seeder)
          return $ ("down" .= statsToJson tz down) :
                   ("up" .= statsToJson tz up) :
                   ("up_seeder" .= statsToJson tz up_seeder) : baseJson
        StatsSwarm -> do
          (seeders, leechers) <- withStats' $ \q ->
            do seeders <- q $ Model.getGauge "seeders" info_hash
               leechers <- q $ Model.getGauge "leechers" info_hash
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
                   ("leechers" .= statsToJson tz leechers) : 
                   baseJson

  -- Our seeder is actually not included in stats
  where cheat :: StatsValue -> StatsValue
        cheat (StatsValue t v) = StatsValue t $ v + 1

statsToJson :: TimeZone -> [StatsValue] -> Value
statsToJson tz = 
    object .
    map (\(StatsValue time val) ->
             (T.pack $ iso8601 $ localTimeToZonedTime tz time) .= val
        )
