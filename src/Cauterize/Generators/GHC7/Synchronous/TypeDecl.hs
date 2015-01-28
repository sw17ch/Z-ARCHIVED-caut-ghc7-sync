{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.TypeDecl
  ( typeDecl
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.GHC7.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import Data.Maybe

typeDecl :: SpType -> Doc
typeDecl (BuiltIn {}) = empty
typeDecl t = td <$> indent 2 der <> line
  where
    td = typeDecl' t
    ders = ["Show", "Eq", "Ord"]
    der = "deriving" <+> parens (hcat $ punctuate ", " ders)

typeDecl' :: SpType -> Doc
typeDecl' (BuiltIn {}) = error "Should never reach this."
typeDecl' t@(Scalar b _ _) =
  let tnd = typeToTypeNameDoc t
  in "newtype" <+> tnd <+> "=" <+> tnd <+> spacedBraces ("un" <> tnd <+> "::" <+> biRepr (scalarRepr b))
typeDecl' t@(Const {}) =
  let tnd = typeToTypeNameDoc t
  in tnd `dataDecl` tnd
typeDecl' t@(Array (TArray _ r _) _ _) =
  let elemName = (sNameToVarNameDoc . typeName) t <> "Elements"
  in arrDecl (typeToTypeNameDoc t) elemName (sNameToTypeNameDoc r)
typeDecl' t@(Vector (TVector _ r _) _ _ _) =
  let elemName = (sNameToVarNameDoc . typeName) t <> "Elements"
  in arrDecl (typeToTypeNameDoc t) elemName (sNameToTypeNameDoc r)
typeDecl' t@(Struct s _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . structFields $ s
  in tnd `dataDecl` tnd <+>
    encloseSep "{ " (line <> "}") ", " (mapMaybe (structFieldDecl prefix) sfs)
typeDecl' t@(Set s _ _ _) =
  let tnd = typeToTypeNameDoc t
      prefix = typeToVarNameDoc t
      sfs = unFields . setFields $ s
  in tnd `dataDecl` tnd <+>
    encloseSep "{ " (line <> "}") ", " (map (setFieldDecl prefix) sfs)
typeDecl' t@(Enum e _ _ _) =
  let tnd = typeToTypeNameDoc t
      rhs = encloseSep " = " empty " | " $ map (enumFieldDecl tnd) (unFields . enumFields $ e)
  in "data" <+> tnd <> rhs
typeDecl' t@(Pad {}) =
  let tnd = typeToTypeNameDoc t
  in "data" <+> tnd <+> "=" <+> tnd

arrDecl :: Doc -> Doc -> Doc -> Doc
arrDecl tName elemName refName =
  tName `dataDecl` tName <+> spacedBraces (elemName <+> ":: V.Vector" <+> refName)

dataDecl :: Doc -> Doc -> Doc
dataDecl lhs rhs = "data" <+> lhs <+> "=" <+> rhs

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
setFieldDecl :: Doc -> Field -> Doc
setFieldDecl nameSpace f =
  case f of
    (EmptyField {}) -> fn' `asType` "Maybe ()"
    (Field _ fr _) -> let fr' = sNameToTypeNameDoc fr
                      in fn' `asType` "Maybe" <+> fr'
  where
    fn' = nameSpace <> sNameToTypeNameDoc (fName f)

enumFieldDecl :: Doc -> Field -> Doc
enumFieldDecl prefix (EmptyField fn _) = prefix <> sNameToTypeNameDoc fn
enumFieldDecl prefix (Field fn fr _) = prefix <> sNameToTypeNameDoc fn <+> sNameToTypeNameDoc fr
