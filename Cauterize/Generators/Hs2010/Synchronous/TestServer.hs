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
    unpackHeader = "aiUnpack" <> aiName <> "Header"
    unpackData = "aiUnpack" <> aiName <> "Data"
    packAll = "aiPack" <> aiName
    headerType = aiName <> "Header"
    header = vcat [ "module Main where"
                  , linebreak
                  , "import Cauterize." <> aiName
                  , "import Cauterize." <> modName
                  , "import Cauterize.TestServer"
                  , linebreak
                  , "main :: IO ()"
                  , "main = do"
                  , "  results <- server iface specHash :: IO [Result" <+> aiName <> "]"
                  , "  print results"
                  , "  where"
                  , "    decHdr b = case" <+> unpackHeader <+> "b of"
                  , "                Right (r," <+> headerType <+> "l t) ->"
                  , "                  Just (HeaderInfo { dataLength = fromIntegral l"
                  , "                                   , dataTag = t"
                  , "                                   , headerRemainder = r"
                  , "                                   })"
                  , "                Left _ -> Nothing"
                  , "    decData b (HeaderInfo l t _) = case" <+> unpackData <+> "(" <> headerType <+> "(fromIntegral l) t) b of"
                  , "                    Right (r,d) ->"
                  , "                      Just (DataInfo { dataResult = d"
                  , "                                     , dataRemainder = r })"
                  , "                    Left _ -> Nothing"
                  , "    encData d = case" <+> packAll <+> "d of"
                  , "                  Right d' -> Just d'"
                  , "                  Left _ -> Nothing"
                  , "    iface = Interface { headerLength = typeTagLength + dataTagLength"
                  , "                      , decodeHeader = decHdr"
                  , "                      , decodeData = decData"
                  , "                      , packAI = encData"
                  , "                      }"
                  ]

