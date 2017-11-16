module Handler.WebSeeder where

import Prelude
import Foundation
import PathPieces

getWebSeedR :: HexInfoHash -> Handler ()
getWebSeedR (HexInfoHash infoHash) = do
  undefined
