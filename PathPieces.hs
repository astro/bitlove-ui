{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PathPieces where

import Prelude
import Yesod (PathPiece (..))
import qualified Data.Text as T

import Utils
import Model.User
import Model.Token

data Period = PeriodDays Int
            | PeriodAll
            deriving (Show, Eq, Read, Ord)
              
instance PathPiece Period where
  fromPathPiece text = 
    case T.unpack text of
      "1" -> Just $ PeriodDays 1
      "7" -> Just $ PeriodDays 7
      "30" -> Just $ PeriodDays 30
      "all" -> Just $ PeriodAll
      _ -> Nothing
  toPathPiece (PeriodDays days) = T.pack $ show days
  toPathPiece PeriodAll = "all"


data TorrentName = TorrentName T.Text
                   deriving (Show, Eq, Read, Ord)
  
instance PathPiece TorrentName where
  fromPathPiece text = do
      let extension = ".torrent"
      l <- case T.length text - T.length extension of
             l' | l' > 0 -> return l'
             _ -> Nothing
      case T.splitAt l text of
        (name, extension') 
            | extension == extension' ->
                return $ TorrentName name
        _ -> Nothing
  toPathPiece (TorrentName name) = name `T.append` ".torrent"
  
  
data StatsPeriod = StatsDay
                 | StatsWeek
                 | StatsMonth
                 | StatsYear
                   deriving (Show, Eq, Read)
                   
instance PathPiece StatsPeriod where
  fromPathPiece "day" = Just StatsDay
  fromPathPiece "week" = Just StatsWeek
  fromPathPiece "month" = Just StatsMonth
  fromPathPiece "year" = Just StatsYear
  fromPathPiece _ = Nothing
  toPathPiece StatsDay = "day"
  toPathPiece StatsWeek = "week"
  toPathPiece StatsMonth = "month"
  toPathPiece StatsYear = "year"
                   
data StatsJSON = StatsSwarm
               | StatsTraffic
               | StatsDownloads
                 deriving (Show, Eq, Read)

instance PathPiece StatsJSON where
    fromPathPiece "swarm.json" = Just StatsSwarm
    fromPathPiece "traffic.json" = Just StatsTraffic
    fromPathPiece "downloads.json" = Just StatsDownloads
    fromPathPiece _ = Nothing
    toPathPiece StatsSwarm = "swarm.json"
    toPathPiece StatsTraffic = "traffic.json"
    toPathPiece StatsDownloads = "downloads.json"

instance PathPiece UserName where
  fromPathPiece = Just . UserName
  toPathPiece (UserName t) = t
  
  
instance PathPiece Token where
  fromPathPiece = Just . Token . fromHex
  toPathPiece = toHex . unToken
  

data Thumbnail = Thumbnail Int
                 deriving (Show, Eq, Ord, Read)

instance PathPiece Thumbnail where
    fromPathPiece "48x48.png" = 
        Just $ Thumbnail 48
    fromPathPiece "64x64.png" = 
        Just $ Thumbnail 64
    fromPathPiece _ =
        Nothing
    toPathPiece (Thumbnail size) =
        let s = T.pack $ show size
        in T.concat [ s
                    , "x"
                    , s
                    , ".png"
                    ]
    