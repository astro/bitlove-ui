{-# LANGUAGE OverloadedStrings #-}
module PathPieces where

import Prelude
import Yesod (PathPiece (..))
import qualified Data.Text as T

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
  