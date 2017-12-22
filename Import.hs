module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Tracked
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    , TorrentName (..)
    , iso8601, rfc822, localTimeToZonedTime
    , fromHex, fromHex', toHex, isHex

    -- Import
    , putStrLn
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last, print, putStr, putStrLn)
import Yesod   hiding (Route(..))
import Foundation
import Tracked (TrackedScrape (..))
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import System.IO (stderr)
import qualified Data.ByteString.Char8 as BC
import Settings.StaticFiles
import Settings.Development
-- TODO: wat?
import PathPieces
import Utils

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

putStrLn :: String -> IO ()
putStrLn = BC.hPutStrLn stderr . BC.pack
