module Cauterize.TestServer (server, Result(..)) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Control.Exception (IOException, catch)

import Data.Word

data Result a = Success a
              | FailBadDecode a
              | FailBadTag [Word8]
              | FailNotEq a a
              | FailSocket
              | FailSpecHash
  deriving (Show, Eq)

server :: (Arbitrary a, Eq a) => [Word8] -> IO [Result a]
server specHash = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "3000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  (conn, _) <- accept sock
  r <- runTests specHash conn
  sClose conn
  sClose sock
  return r

runTests :: (Arbitrary a, Eq a) => [Word8] -> Socket -> IO [Result a]
runTests h s = work `catch` orFail
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

          -- Here's the part where I need to write the header out, write out
          -- the data, then read the header and read the data.
          _ <- send s $ B.pack [0x44, 0x45, 0x46] -- this is a dummy send for now

          return [Success r]

recvExactly :: Socket -> Int -> IO B.ByteString
recvExactly s len = do
  d <- recv s len
  let rlen = B.length d
  if rlen == len
    then return d
    else do
      r <- recvExactly s (len - rlen)
      return $ d `B.append` r
