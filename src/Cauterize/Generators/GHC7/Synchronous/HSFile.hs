{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.HSFile
  ( hsFileName
  , libName
  , renderHSFile
  ) where

import Cauterize.Specification

import Cauterize.Generators.GHC7.Synchronous.Common
import Cauterize.Generators.GHC7.Synchronous.TypeDecl
import Cauterize.Generators.GHC7.Synchronous.TypeSize
import Cauterize.Generators.GHC7.Synchronous.TypePack
import Cauterize.Generators.GHC7.Synchronous.Functions
import Cauterize.Generators.GHC7.Synchronous.Special

import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text


renderHSFile :: Spec -> T.Text
renderHSFile s = displayT . r $ hsMod <> linebreak <$> parts
  where
    tm = specTypeMap s
    n = specName s
    r = renderPretty 0.4 80
    ts = specTypes s
    parts = vcat [ imports
                 , linebreak
                 , functions s
                 , linebreak
                 , typeDecls
                 , linebreak
                 , typeSizers
                 , linebreak
                 , typePackers
                 , linebreak
                 , typeArbInst
                 , linebreak
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <+> "where"

    imports = vcat [ "import qualified Data.Vector as V"
                   , "import Cauterize.Generators.GHC7.SynchronousSupport"
                   , "import Control.Monad"
                   , "import Control.Monad.Trans"
                   , "import Control.Monad.Trans.Except"
                   , "import Data.Maybe"
                   , "import Data.Word"
                   , "import qualified Data.Serialize.Get as S"
                   , "import qualified Data.Serialize.Put as P"
                   , "import qualified Data.ByteString as B"
                   , "import Data.Bits as Bits"
                   , "import qualified Test.QuickCheck as QC"
                   ]

    typeDecls = vcat $ map typeDecl ts
    typeSizers = vcat $ map (typeSizer tm) ts
    typePackers = vcat $ map (typePacker tm) ts
    typeArbInst = vcat $ map arbInst ts
