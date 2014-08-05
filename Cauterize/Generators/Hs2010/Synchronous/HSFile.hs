{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.HSFile
  ( hsFileName
  , renderHSFile
  ) where

import Cauterize.Specification

import Cauterize.Generators.Hs2010.Synchronous.Common
import Cauterize.Generators.Hs2010.Synchronous.TypeDecl
import Cauterize.Generators.Hs2010.Synchronous.TypeSize

import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text


renderHSFile :: Spec -> T.Text
renderHSFile s = displayT . r $ hsMod <> linebreak <$> parts
  where
    n = T.pack $ specName s
    r = renderPretty 0.4 80
    ts = specTypes s
    parts = vcat [ imports
                 , linebreak
                 , typeDecls
                 , linebreak
                 , typeSizers
                 , linebreak
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <+> "where"

    imports = vcat [ "import Data.Maybe"
                   , "import Data.Serialize"
                   , "import Data.Word"
                   , "import Data.Int"
                   , "import qualified Data.Vector as V"
                   , "import Cauterize.Support.Hs2010"
                   ]

    typeDecls = vcat $ map typeDecl ts
    typeSizers = vcat $ map typeSizer ts
