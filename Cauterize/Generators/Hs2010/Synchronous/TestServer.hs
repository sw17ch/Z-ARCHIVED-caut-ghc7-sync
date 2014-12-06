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
    aiName = (text . nameToCapHsName . T.pack . AI.aiName) ai <> "AI"
    header = vcat [ "module Main where"
                  , linebreak
                  , "import Cauterize." <> aiName
                  , "import Network.Socket"
                  , "import Network.Socket.ByteString"
                  , "import Test.QuickCheck.Arbitrary"
                  , "import Test.QuickCheck.Gen"
                  , linebreak
                  , "main :: IO ()"
                  , "main = withSocketsDo $ do"
                  , "  putStrLn \"" <> aiName <> " test server.\""
                  ]
