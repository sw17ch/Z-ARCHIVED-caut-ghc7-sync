{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Cauterize.Generators.Hs2010.Synchronous.HSFile where

import Cauterize.Specification

import qualified Data.Char as C
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

hsFileName :: Spec -> FilePath
hsFileName s = let part = specNameToHsName $ T.pack $ specName s :: T.Text
                   suff = ".hs"
               in T.unpack $ part `T.append` suff

specNameToHsName :: T.Text -> T.Text
specNameToHsName n = let terms = T.splitOn "_" n
                         cap (T.length -> 0) = ""
                         cap t = C.toUpper (T.head t) `T.cons` T.tail t
                      in T.concat $ map cap terms

renderHSFile :: Spec -> T.Text
renderHSFile s = displayT . r $ hsMod <> linebreak <$> imports
  where
    n = T.pack $ specName s
    r = renderPretty 0.4 80

    hsMod = "module Cauterize." <> (text . specNameToHsName $ n) <+> "where"

    imports = vcat [ "import Data.Maybe"
                   , "import Data.Serialize"
                   , linebreak
                   ]
