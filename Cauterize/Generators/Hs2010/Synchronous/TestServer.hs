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
                  , "import Cauterize.TestServer"
                  , linebreak
                  , "main :: IO ()"
                  , "main = do"
                  , "  results <- server specHash :: IO [Result" <+> aiName <> "]"
                  , "  print results"
                  ]

