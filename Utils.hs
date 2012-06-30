module Utils (iso8601) where

import Prelude
import Data.Time
import System.Locale


iso8601 :: LocalTime -> String
iso8601 time =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") time ++
    zone
    where zone = case formatTime defaultTimeLocale "%z" time of
                   (sig:h1:h2:m1:m2) ->
                     sig:h1:h2:':':m1:m2
                   _ ->
                     "Z"
