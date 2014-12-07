{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.TestServer
  ( hsTsFileName
  , renderTsFile
  ) where

import qualified Cauterize.AI as AI
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text
import Cauterize.Generators.Hs2010.Synchronous.Common

renderTsFile :: AI.Ai -> T.Text
renderTsFile ai = displayT . renderPretty 0.6 160 $ header
  where
    modName = (text . nameToCapHsName . T.pack . AI.aiName) ai 
    aiName = modName <> "AI"
    header = vcat [ "module Main where"
                  , linebreak
                  , "import Cauterize." <> aiName
                  , "import Cauterize." <> modName
                  , "import Network.Socket hiding (send, recv)"
                  , "import Network.Socket.ByteString"
                  , "import Test.QuickCheck.Arbitrary"
                  , "import Test.QuickCheck.Gen"
                  , linebreak
                  , "main :: IO ()"
                  , "main = withSocketsDo $ do"
                  , "  putStrLn \"" <> aiName <> " test server.\""
                  , "  addrinfos <- getAddrInfo"
                  , "               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))"
                  , "               Nothing (Just \"3000\")"
                  , "  let serveraddr = head addrinfos"
                  , "  sock <- socket (addrFamily serveraddr) Stream defaultProtocol"
                  , "  bindSocket sock (addrAddress serveraddr)"
                  , "  listen sock 1"
                  , "  (conn, _) <- accept sock"
                  , "  runTests conn"
                  , "  sClose conn"
                  , "  sClose sock"
                  , linebreak
                  , "runTests :: Socket -> IO ()"
                  , "runTests s = do"
                  , "  msg <- recv s hashLen"
                  , "  print msg"
                  ]

