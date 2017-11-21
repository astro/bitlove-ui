{-# LANGUAGE OverloadedStrings #-}
module Handler.TorrentStats where

import Yesod
import Data.Time
import qualified Data.Text as T

import Import
import PathPieces
import qualified Model
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
          withStats :: (LocalTime -> LocalTime -> Int -> Transaction a)
                     -> Handler a
          -- |Can run one query
          withStats f = withDB $
                        f localStart localStop interval
          -- |Supplies `q` for multiple queries
          withStats' :: (((LocalTime -> LocalTime -> Int -> Transaction a) -> IO a) -> IO b) -> Handler b
          withStats' f = withDB $ \db ->
                         f $ \f' ->
                         f' localStart localStop interval db

          counterToValue q name =
            statsToJson tz <$>
            q (Model.getCounter name info_hash)
          counterToPair q name =
            (name .=) <$>
            counterToValue q name

      (RepJson . toContent . object) <$> case stats of
        StatsDownloads -> do
          canEdit' <- canEdit user
          urlRender <- getUrlRender

          withStats' $ \q -> do
            json <- mapM (\(key, name) ->
                            (key .=) <$>
                            counterToValue q name
                         )
                    [("downloads", "complete"),
                     ("downloads_w", "complete_w")]
            ownerJson <-
              if canEdit'
              then do
                let downloadCounterToPair q handler name =
                      let path =
                            urlRender $ handler user slug $ TorrentName name
                      in
                        (name .=) <$>
                        statsToJson tz <$>
                        q (Model.getDownloadCounter path)
                mapM (uncurry $ downloadCounterToPair q)
                  [(TorrentFileR, "torrent"),
                   (TorrentFileForWebtorrentR, "torrent_w")
                  ]
              else return []
            return $ json ++ ownerJson ++ baseJson
        StatsTraffic ->
          withStats' $ \q -> do
          (++ baseJson) <$>
            mapM (counterToPair q)
            ["down", "up", "up_seeder", "down_w", "up_w", "up_seeder_w"]
        StatsSwarm ->
          withStats' $ \q ->
          do let completeGauge vs =
                     completeGauge' $
                     case vs of
                       -- First beyond start?
                       StatsValue t value : _
                           | t > localStart &&
                             value > 0 ->
                               let t' = utcToLocalTime tz $
                                        fromIntegral (-interval) `addUTCTime` localTimeToUTC tz t
                               in StatsValue t' 0 : vs
                       _ ->
                         vs
                 -- empty gauge
                 completeGauge' [] = []
                 -- last before stop?
                 completeGauge' [v@(StatsValue t value)]
                     | t < localStop = [v, StatsValue localStop value]
                 -- traverse till last (above case)
                 completeGauge' (v:vs) = v : completeGauge' vs

                 gaugeToValue name isSeeders =
                   (name .=) <$>
                   statsToJson tz <$>
                   completeGauge <$>
                   (if isSeeders then map cheat else id) <$>
                   q (Model.getGauge name info_hash)

             (++ baseJson) <$>
               mapM (uncurry gaugeToValue)
               [("seeders", True),
                ("leechers", False),
                ("seeders_w", True),
                ("leechers_w", False)
               ]

  -- Our seeder is actually not included in stats
  where cheat :: StatsValue -> StatsValue
        cheat (StatsValue t v) = StatsValue t $ v + 1

statsToJson :: TimeZone -> [StatsValue] -> Value
statsToJson tz =
    object .
    map (\(StatsValue time val) ->
             T.pack (iso8601 $ localTimeToZonedTime tz time) .= val
        )
