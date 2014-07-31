{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Cauterize.Generators.Hs2010.Synchronous.HSFile
  ( hsFileName
  , renderHSFile
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Common.Primitives
import Cauterize.Common.Field

import qualified Data.Char as C
import qualified Data.Text.Lazy as T
import Data.Maybe
import Text.PrettyPrint.Leijen.Text

hsFileName :: Spec -> FilePath
hsFileName s = let part = nameToCapHsName $ T.pack $ specName s :: T.Text
                   suff = ".hs"
               in T.unpack $ part `T.append` suff

nameToCapHsName :: T.Text -> T.Text
nameToCapHsName n = let terms = T.splitOn "_" n
                        cap (T.length -> 0) = ""
                        cap t = C.toUpper (T.head t) `T.cons` T.tail t
                    in T.concat $ map cap terms

nameToHsName :: T.Text -> T.Text
nameToHsName n = let (f:rest) = T.splitOn "_" n
                     cap (T.length -> 0) = ""
                     cap t = C.toUpper (T.head t) `T.cons` T.tail t
                 in T.concat $ f:map cap rest

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

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <+> "where"

    imports = vcat [ "import Data.Maybe"
                   , "import Data.Serialize"
                   , "import Data.Word"
                   , "import Data.Int"
                   , "import Data.Vector"
                   ]

    typeDecls = vcat $ map typeDecl ts

typeDecl :: SpType -> Doc
typeDecl t@(BuiltIn b _ _) = "type" <+> typeToTypeNameDoc t <+> "=" <+> biRepr (unTBuiltIn b)
typeDecl t@(Scalar b _ _) =
  let tnd = typeToTypeNameDoc t
  in "newtype" <+> tnd <+> "=" <+> tnd <+> spacedBraces ("un" <> tnd <+> "::" <+> biRepr (scalarRepr b))
typeDecl t@(Const {}) =
  let tnd = typeToTypeNameDoc t
  in "data" <+> tnd <+> "=" <+> tnd
typeDecl t@(FixedArray (TFixedArray _ r _) _ _) =
  let tnd = typeToTypeNameDoc t
      elemName = (sNameToVarNameDoc . typeName) t <> "Elements"
      rnd = sNameToTypeNameDoc r
  in "data" <+> tnd <+> "=" <+> tnd <+> spacedBraces (elemName <+> ":: Vector" <+> rnd)
typeDecl t@(BoundedArray (TBoundedArray _ r _) _ _ repr) =
  let tnd = typeToTypeNameDoc t
      rnd = sNameToTypeNameDoc r
      fieldPrefix = typeToTypeNameDoc t
      repnd = biRepr . unLengthRepr $ repr
  in "data" <+> tnd <+> "=" <+> tnd <+>
      encloseSep "{ " (line <> "}") ", " [ fieldPrefix <> "Length ::" <+> repnd
                                         , fieldPrefix <> "Elements :: Vector" <+> rnd]
typeDecl t@(Struct s _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . structFields $ s 
  in "data" <+> tnd <+> "=" <+> tnd <+> encloseSep "{ " (line <> "}") ", " (mapMaybe (structFieldDecl prefix) sfs)
typeDecl t@(Set s _ _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . setFields $ s 
  in "data" <+> tnd <+> "=" <+> tnd <+> encloseSep "{ " (line <> "}") ", " (mapMaybe (setFieldDecl prefix) sfs)
typeDecl t@(Enum e _ _ _) =
  let tnd = typeToTypeNameDoc t
      rhs = encloseSep " = " empty " | " $ map (enumFieldDecl tnd) (unFields . enumFields $ e)
  in "data" <+> tnd <> rhs
typeDecl t@(Partial e _ _ _ _) =
  let tnd = typeToTypeNameDoc t
      rhs = encloseSep " = " empty " | " $ map (enumFieldDecl tnd) (unFields . partialFields $ e)
  in "data" <+> tnd <> rhs
typeDecl t@(Pad {}) =
  let tnd = typeToTypeNameDoc t
  in "data" <+> tnd <+> "=" <+> tnd

-- TODO: Should we attempt to drop some annotation in the Haskell code that an
-- empty field was left out?
structFieldDecl :: Doc -> Field -> Maybe Doc
structFieldDecl _ (EmptyField _ _) = Nothing
structFieldDecl nameSpace (Field fn fr _) =
  let fn' = nameSpace <> sNameToTypeNameDoc fn
      fr' = sNameToTypeNameDoc fr
  in Just $ fn' `asType` fr'

-- TODO: Should we attempt to drop some annotation in the Haskell code that an
-- empty field was left out?
setFieldDecl :: Doc -> Field -> Maybe Doc
setFieldDecl _ (EmptyField _ _) = Nothing
setFieldDecl nameSpace (Field fn fr _) =
  let fn' = nameSpace <> sNameToTypeNameDoc fn
      fr' = sNameToTypeNameDoc fr
  in Just $ fn' `asType` "Maybe" <+> fr'

enumFieldDecl :: Doc -> Field -> Doc
enumFieldDecl prefix (EmptyField fn _) = prefix <> sNameToTypeNameDoc fn
enumFieldDecl prefix (Field fn fr _) = prefix <> sNameToTypeNameDoc fn <+> sNameToTypeNameDoc fr


asType :: Doc -> Doc -> Doc
asType lhs rhs = lhs <+> "::" <+> rhs

sNameToTypeNameDoc :: String -> Doc
sNameToTypeNameDoc = text . nameToCapHsName . T.pack

sNameToVarNameDoc :: String -> Doc
sNameToVarNameDoc = text . nameToHsName . T.pack

typeToTypeNameDoc :: SpType -> Doc
typeToTypeNameDoc = sNameToTypeNameDoc . typeName

typeToVarNameDoc :: SpType -> Doc
typeToVarNameDoc = sNameToVarNameDoc . typeName

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

spacedBraces :: Doc -> Doc
spacedBraces p = braces $ " " <> p <> " "
