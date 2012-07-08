module Handler.Tracker where

import Yesod
import qualified Network.Wai as Wai
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC

import Import
import qualified Model as Model
import qualified Benc as Benc


newtype RepBenc = RepBenc Benc.BValue

instance HasReps RepBenc where
    chooseRep (RepBenc v) _ = 
        return ("application/x-bittorrent",
                ContentBuilder (Benc.toBuilder v) Nothing
               )

-- TODO: insert own seeder!
-- TODO: support key parameter
getAnnounceR :: Handler RepBenc
getAnnounceR = do
  query <- getRawQuery
  let q :: BC.ByteString -> Maybe BC.ByteString
      q = join . (`lookup` query)
      qi :: Read r => BC.ByteString -> Maybe r
      qi name = do v <- qi name
                   case readsPrec 0 v of
                     [(r, "")] -> return r
                     _ -> Nothing
      mTr = TrackerRequest <$>
            Model.InfoHash <$> q "info_hash" <*>
            q "peer_id" <*>
            qi "port" <*>
            qi "uploaded" <*>
            qi "downloaded" <*>
            qi "left" <*>
            pure (q "event") <*>
            pure (maybe False (const True) $ q "compact")
  case mTr of
    Nothing ->
        return $ RepBenc $
        Benc.BDict [(Benc.BString "failure",
                     Benc.BString "Invalid tracker request"),
                    (Benc.BString "interval",
                     Benc.BInt 0xffff)]
    Just tr ->
        do let isSeeder = trLeft tr == 0
           Model.getPeers tr

getScrapeR :: Handler RepBenc
getScrapeR = undefined

getRawQuery :: Handler [(BC.ByteString, Maybe BC.ByteString)]
getRawQuery = 
    Wai.queryString `fmap`
    waiRequest
    
    