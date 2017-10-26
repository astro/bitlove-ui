module Tracker.Utils where

import Prelude
import Yesod
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.Socket (SockAddr (..), HostName, getAddrInfo, addrAddress, tupleToHostAddress, tupleToHostAddress6)
import Data.Word (Word32)
import Data.Bits

import Model.Tracker (PeerAddress(..))


getRemoteAddr :: HandlerT a IO PeerAddress
getRemoteAddr = do
  req <- waiRequest
  let remote = normalize $ Wai.remoteHost req
  toPeerAddress <$>
    case isLocalhost remote of
      False -> return remote
      True ->
        liftIO $
        fromMaybe (return remote) $
        fmap (parseAddr . BC.unpack) $
        "X-Real-IP" `lookup` Wai.requestHeaders req

      where
        normalize (SockAddrInet6 port _ (0, 0, 0xffff, haddr) _) =
          SockAddrInet port haddr
        normalize sockaddr =
          sockaddr

        toPeerAddress (SockAddrInet _ haddr) =
          Peer4 $ wordToByteString haddr
        toPeerAddress (SockAddrInet6 _ _ (h6, h6', h6'', h6''') _) =
          Peer6 $ B.concat [ wordToByteString h6
                           , wordToByteString h6'
                           , wordToByteString h6''
                           , wordToByteString h6'''
                           ]
        toPeerAddress _ =
          error "Must not happen"

        isLocalhost (SockAddrInet _ addr)
          | addr == tupleToHostAddress (127, 0, 0, 1) =
              True
        isLocalhost (SockAddrInet6 _ _ addr _)
          | addr == tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 1) =
              True
        isLocalhost _ =
          False

        parseAddr :: HostName -> IO SockAddr
        parseAddr s =
          addrAddress <$>
          head <$>
          getAddrInfo Nothing (Just s) Nothing

wordToByteString :: Word32 -> B.ByteString
wordToByteString w =
    B.pack $
    map (fromIntegral . (.&. 0xFF) . (w `shiftR`) . (* 8)) $
    reverse [0..3]

portToByteString :: Int -> B.ByteString
portToByteString p =
  B.pack $
  map (fromIntegral . (.&. 0xFF)) $
  [ p `shiftR` 8
  , p
  ]
