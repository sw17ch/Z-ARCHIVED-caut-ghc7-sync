{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.TestServer
  ( hsTsFileName
  , renderTsFile
  ) where

import qualified Cauterize.Meta as M
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text
import Cauterize.Generators.GHC7.Synchronous.Common

renderTsFile :: M.Meta -> T.Text
renderTsFile meta = displayT . renderPretty 0.6 160 $ header
  where
    modName = (text . nameToCapHsName . M.metaName) meta
    metaName = modName <> "Meta"
    unpackHeader = "aiUnpack" <> metaName <> "Header"
    unpackData = "aiUnpack" <> metaName <> "Data"
    packAll = "aiPack" <> metaName
    headerType = metaName <> "Header"
    header = vcat [ "module Main where"
                  , linebreak
                  , "import Cauterize." <> metaName
                  , "import Cauterize." <> modName
                  , "import Cauterize.TestServer"
                  , linebreak
                  , "main :: IO ()"
                  , "main = do"
                  , "  results <- server iface specHash :: IO [Result" <+> metaName <> "]"
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

