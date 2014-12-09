module Cauterize.TestServer
  ( server
  , Result(..)
  , HeaderInfo(..)
  , Interface(..)
  , DataInfo(..)
  ) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Control.Exception (IOException, catch)

import Data.Word

data HeaderInfo = HeaderInfo { dataLength :: Int, dataTag :: [Word8], headerRemainder :: B.ByteString }
  deriving (Show)

data DataInfo a = DataInfo { dataResult :: a, dataRemainder :: B.ByteString }
  deriving (Show)

data Interface a = Interface { headerLength :: Int
                             , decodeHeader :: B.ByteString -> Maybe HeaderInfo
                             , decodeData :: B.ByteString -> HeaderInfo -> Maybe (DataInfo a)
                             , packAI :: a -> Maybe B.ByteString
                             }

data Result a = Success a
              | FailBadDecodeHeader a
              | FailBadDecodeData a
              | FailBadTag [Word8]
              | FailNotEq a a
              | FailEncode
              | FailSocket
              | FailSpecHash
  deriving (Show, Eq)

server :: (Arbitrary a, Eq a) => Interface a -> [Word8] -> IO [Result a]
server iface specHash = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "3000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  (conn, _) <- accept sock
  r <- runTests iface specHash conn
  sClose conn
  sClose sock
  return r

runTests :: (Arbitrary a, Eq a) => Interface a -> [Word8] -> Socket -> IO [Result a]
runTests iface h s = work `catch` orFail
  where
    orFail :: IOException -> IO [Result a]
    orFail _ = return [FailSocket]

    hashLen = length h

    work = do
      remoteHash <- recvExactly s hashLen
      if B.unpack remoteHash /= h
        then return [FailSpecHash]
        else do
          r <- generate arbitrary
          let bin = packAI iface r
          case bin of
             Nothing -> return [FailEncode]
             Just bin' -> do
               _ <- send s bin'
               hdrBin <- recvExactly s (headerLength iface)
               let hdr = decodeHeader iface hdrBin
               case hdr of
                  Nothing -> return [FailBadDecodeHeader r]
                  Just hdr' -> do
                    datBin <- recvExactly s (dataLength hdr')
                    let dat = decodeData iface datBin hdr'
                    case dat of
                      Nothing -> return [FailBadDecodeData r]
                      Just (DataInfo dat' _) -> return [Success dat']

recvExactly :: Socket -> Int -> IO B.ByteString
recvExactly s len = do
  d <- recv s len
  let rlen = B.length d
  if rlen == len
    then return d
    else do
      r <- recvExactly s (len - rlen)
      return $ d `B.append` r
