{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Cauterize.Generators.Hs2010.Synchronous.HSFile where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Common.Primitives

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
renderHSFile s = displayT . r $ hsMod <> linebreak <$> parts
  where
    n = T.pack $ specName s
    r = renderPretty 0.4 80
    ts = specTypes s
    parts = vcat [ imports
                 , linebreak
                 , typeDecls
                 , linebreak
                 ]

    hsMod = "module Cauterize." <> (text . specNameToHsName $ n) <+> "where"

    imports = vcat [ "import Data.Maybe"
                   , "import Data.Serialize"
                   , "import Data.Word"
                   , "import Data.Int"
                   , "import Data.Vector"
                   ]

    typeDecls = vcat $ map typeDecl ts

typeDecl :: SpType -> Doc
typeDecl t@(BuiltIn b _ _) = "type" <+> typeNameDoc t <+> "=" <+> biRepr (unTBuiltIn b)
typeDecl t@(Scalar b _ _) = let tnd = typeNameDoc t
                            in "newtype" <+> tnd <+> "=" <+> tnd <+> "{ un" <> tnd <+> "::" <+> biRepr (scalarRepr b) <+> "}"
typeDecl t@(Const {}) = let tnd = typeNameDoc t
                        in "data" <+> tnd <+> "=" <+> tnd
typeDecl t@(FixedArray (TFixedArray _ r _) _ _) =
  let tnd = typeNameDoc t
      rnd = text . specNameToHsName . T.pack $ r
  in "data" <+> tnd <+> "=" <+> tnd <+> parens ("Vector" <+> rnd)
typeDecl t@(BoundedArray (TBoundedArray _ r _) _ _ repr) =
  let tnd = typeNameDoc t
      rnd = text . specNameToHsName . T.pack $ r
      repnd = biRepr . unLengthRepr $ repr
  in "data" <+> tnd <+> "=" <+> tnd <+> repnd <+> parens ("Vector" <+> rnd)
typeDecl t = "data" <+> typeNameDoc t <+> "= ???"

typeNameDoc :: SpType -> Doc
typeNameDoc t = text . specNameToHsName . T.pack $ typeName t

biRepr :: BuiltIn -> Doc
biRepr BIu8 = "Word8"
biRepr BIu16 = "Word16"
biRepr BIu32 = "Word32"
biRepr BIu64 = "Word64"
biRepr BIs8 = "Int8"
biRepr BIs16 = "Int16"
biRepr BIs32 = "Int32"
biRepr BIs64 = "Int64"
biRepr BIieee754s = "Float"
biRepr BIieee754d = "Double"
biRepr BIbool = "Bool"
