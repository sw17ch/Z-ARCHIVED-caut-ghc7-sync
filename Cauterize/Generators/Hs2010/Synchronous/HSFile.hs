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
                 , typeSizers
                 , linebreak
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <+> "where"

    imports = vcat [ "import Data.Maybe"
                   , "import Data.Serialize"
                   , "import Data.Word"
                   , "import Data.Int"
                   , "import qualified Data.Vector as V"
                   ]

    typeDecls = vcat $ map typeDecl ts
    typeSizers = vcat $ map typeSizer ts

typeDecl :: SpType -> Doc
typeDecl t@(BuiltIn b _ _) = "type" <+> typeToTypeNameDoc t <+> "=" <+> biRepr (unTBuiltIn b)
typeDecl t@(Scalar b _ _) =
  let tnd = typeToTypeNameDoc t
  in "newtype" <+> tnd <+> "=" <+> tnd <+> spacedBraces ("un" <> tnd <+> "::" <+> biRepr (scalarRepr b))
typeDecl t@(Const {}) =
  let tnd = typeToTypeNameDoc t
  in tnd `dataDecl` tnd
typeDecl t@(FixedArray (TFixedArray _ r _) _ _) =
  let tnd = typeToTypeNameDoc t
      elemName = (sNameToVarNameDoc . typeName) t <> "Elements"
      rnd = sNameToTypeNameDoc r
  in tnd `dataDecl` tnd <+> spacedBraces (elemName <+> ":: Vector" <+> rnd)
typeDecl t@(BoundedArray (TBoundedArray _ r _) _ _ repr) =
  let tnd = typeToTypeNameDoc t
      rnd = sNameToTypeNameDoc r
      fieldPrefix = typeToVarNameDoc t
      repnd = biRepr . unLengthRepr $ repr
  in tnd `dataDecl` tnd <+>
      encloseSep "{ " (line <> "}") ", " [ fieldPrefix <> "Length ::" <+> repnd
                                         , fieldPrefix <> "Elements :: Vector" <+> rnd]
typeDecl t@(Struct s _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . structFields $ s 
  in tnd `dataDecl` tnd <+>
    encloseSep "{ " (line <> "}") ", " (mapMaybe (structFieldDecl prefix) sfs)
typeDecl t@(Set s _ _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . setFields $ s 
  in tnd `dataDecl` tnd <+>
    encloseSep "{ " (line <> "}") ", " (mapMaybe (setFieldDecl prefix) sfs)
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

typeSizer :: SpType -> Doc
typeSizer (BuiltIn (TBuiltIn t) _ s) = staticSize (biReprText t) s
typeSizer (Scalar (TScalar n _) _ s) = staticSize (T.pack n) s
typeSizer (Const (TConst n _ _) _ s) = staticSize (T.pack n) s
typeSizer (FixedArray (TFixedArray n _ _) _ _) =
  sizerInstance n (parens (sNameToTypeNameDoc n <+> "vec")) " = (V.sum . V.map typeSizer) vec"
typeSizer (BoundedArray (TBoundedArray n _ _) _ _ _) =
  sizerInstance n (parens (sNameToTypeNameDoc n <+> "len" <+> "vec")) " = typeSizer len + (V.sum . V.map typeSizer) vec"
typeSizer (Struct (TStruct n (Fields fs)) _ _) =
  let flabels = fieldArgs fs
  in sizerInstance n (matchFields n flabels)
                     (encloseSep " = " empty " + " (map (\x -> text $ "typeSizer " `T.append` x) flabels))
typeSizer (Set (TSet n (Fields fs)) _ _ _) =
  let flabels = fieldArgs fs
  in sizerInstance n (withTypeMatchFields "t" n flabels)
                     (encloseSep " = " empty " + " ("(cautSize . tagRepr) t" : map (\x -> text $ "maybe 0 typeSizer " `T.append` x) flabels))
typeSizer t@(Enum (TEnum n (Fields fs)) _ _ _) =
  let sizedFields = align . vcat $ map (enumFieldSizer (typeToTypeNameDoc t)) fs
      rhs = align $ "let" <+> (align . vcat) [ "tags = cautSize . tagRepr $ t"
                                     , "typs = case e of" <$> indent 12 sizedFields
                                     ]
                    <$> "in tags + typs"
  in sizerInstance n "e" $ " = " <> rhs

typeSizer t = sizerInstance (typeName t) "(TheType f)" "= ?????????????????????????????"

enumFieldSizer :: Doc -> Field -> Doc
enumFieldSizer prefix (EmptyField n _) = prefix <> sNameToTypeNameDoc n <+> "-> 0"
enumFieldSizer prefix (Field n _ _) = prefix <> sNameToTypeNameDoc n <+> "t -> typeSizer t"

withTypeMatchFields :: Doc -> String -> [T.Text] -> Doc
withTypeMatchFields t n flabels = t <> "@" <> parens (sNameToTypeNameDoc n <+> (text . T.unwords) flabels)

matchFields :: String -> [T.Text] -> Doc
matchFields n flabels = parens $ sNameToTypeNameDoc n <+> (text . T.unwords) flabels

fieldArgs :: [a] -> [T.Text]
fieldArgs fs = take (length fs) $ map (("f" `T.append`) . T.pack . show) ([0..] :: [Integer])

sizerInstance :: String -> Doc -> Doc -> Doc
sizerInstance n pat rhs =
  vcat [ "instance CauterizeSize" <+> sNameToTypeNameDoc n <+> "where"
       , indent 2 $ "cautSize" <+> pat <> rhs
       ]

fixedSizeDoc :: FixedSize -> Doc
fixedSizeDoc = text . T.pack . show . unFixedSize

staticSize :: T.Text -> FixedSize -> Doc
staticSize n s =
  vcat [ "instance CauterizeSize" <+> (text . nameToCapHsName) n <+> "where"
       , indent 2 "cautSize _ =" <+> fixedSizeDoc s
       ]

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

dataDecl :: Doc -> Doc -> Doc
dataDecl lhs rhs = "data" <+> lhs <+> "=" <+> rhs

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
biRepr = text . biReprText

biReprText :: BuiltIn -> T.Text
biReprText BIu8 = "Word8"
biReprText BIu16 = "Word16"
biReprText BIu32 = "Word32"
biReprText BIu64 = "Word64"
biReprText BIs8 = "Int8"
biReprText BIs16 = "Int16"
biReprText BIs32 = "Int32"
biReprText BIs64 = "Int64"
biReprText BIieee754s = "Float"
biReprText BIieee754d = "Double"
biReprText BIbool = "Bool"

spacedBraces :: Doc -> Doc
spacedBraces p = braces $ " " <> p <> " "
