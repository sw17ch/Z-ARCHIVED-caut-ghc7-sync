module Cauterize.TestServer where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Word

data Result a = Success a
              | FailBadDecode a
              | FailBadTag [Word8]
              | FailNotEq a a
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
runTests h s = do
  msg <- recv s hashLen
  _ <- send s msg
  print msg
  r <- generate arbitrary
  return [Success r]
  where
    hashLen = length h
