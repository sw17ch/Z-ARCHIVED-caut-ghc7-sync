{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.TypeSize
  ( typeSizer
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M

sizeFn :: Doc
sizeFn = "cautSize"

minSizeFromSize :: (Sized s) => s -> Doc
minSizeFromSize s = "minSize _ = " <> integer (minSize s)

maxSizeFromSize :: (Sized s) => s -> Doc
maxSizeFromSize s = "maxSize _ = " <> integer (maxSize s)

typeSizer :: M.Map Name SpType -> SpType -> Doc
typeSizer _ (BuiltIn {}) = empty
typeSizer m t = let n = typeName t
                    inst = "instance CauterizeSize" <+> sNameToTypeNameDoc n <+> "where" <$> indent 2 (typeSizer' m t)
                in inst <> linebreak
                 
typeSizer' :: M.Map Name SpType -> SpType -> Doc
typeSizer' _ (BuiltIn {}) = error "Should never reach this."
typeSizer' _ (Scalar s _ _) = hsep [sizeFn, unpackScalarAs s "x", "=", sizeFn, "x"]
typeSizer' _ (Const c _ _) = hsep [sizeFn, "_ =", sizeFn, constAsUndefined c]
typeSizer' _ t@(Array (TArray _ _ l) _ s) =
  varSizeInsts s $ 
    "cautSize (" <> typeToTypeNameDoc t <> " v) = "
     <> ifStmt (integer l <> " /= V.length v")
               "Nothing"
               "liftM V.sum $ V.mapM cautSize v"
typeSizer' _ t@(Vector (TVector _ _ l) _ s (LengthRepr r)) =
  varSizeInsts s $ 
    "cautSize (" <> typeToTypeNameDoc t <> " v) = "
     <> ifStmt (integer l <> " < V.length v") "Nothing" e
  where
    e = "do" <+> align (vcat [ "lengthSize <- cautSize" <+> builtinAsUndefined r
                             , "elemSize <- liftM V.sum $ V.mapM cautSize v"
                             , "return $ lengthSize + elemSize"
                             ])
typeSizer' _ (Struct (TStruct n (Fields fs)) _ s) =
  let objName = "s"
      fsizes = map (structFieldSizer objName $ sNameToVarNameDoc n) fs
      doblk = "liftM sum $ sequence " <+> align (encloseSep "[ " (line <> "]") ", " fsizes)
  in varSizeInsts s $ "cautSize" <+> objName <+> "=" <+> doblk
typeSizer' _ (Set (TSet n (Fields fs)) _ s (FlagsRepr r)) =
  let objName = "s"
      repName = biRepr r
      fsizes = map (setFieldSizer objName $ sNameToVarNameDoc n) fs
  in varSizeInsts s $
    "cautSize" <+> objName <+> "=" <+>
      "do" <+> align (vcat [ "flagsSize <- cautSize (undefined :: " <> repName <> ")"
                           , "fieldsSize <- liftM sum $ sequence" <+>
                              align (encloseSep "[ " (line <> "]") ", " fsizes)
                           , "return $ flagsSize + fieldsSize"
                           ])
typeSizer' _ (Enum (TEnum n (Fields fs)) _ s (TagRepr r)) =
  let objName = "e"
      fieldVar = "e'"
      repName = biRepr r
      enumCase f = let lhs = enumFieldCaseMatch fieldVar (sNameToTypeNameDoc n) f
                       rhs = enumFieldSizer fieldVar f
                   in lhs <+> rhs
      fcases = map enumCase fs
      caseBlob = align $ "e of" <$> vcat fcases
      e = "do" <+> align ( "repSize <- cautSize (undefined :: " <> repName <> ")"
                       <$> "fieldsSize <- case " <> align caseBlob
                       <$> "return $ repSize + fieldsSize")
  in varSizeInsts s $ "cautSize" <+> objName <+> "=" <+> e
typeSizer' _ (Pad (TPad _ l) _ _) =
  vcat [ "cautSize _ = Just " <> integer l
       ] 

enumFieldCaseMatch :: Doc -> Doc -> Field -> Doc
enumFieldCaseMatch _ nameSpace (EmptyField n _) = nameSpace <> sNameToTypeNameDoc n <+> "->"
enumFieldCaseMatch var nameSpace (Field n _ _) = nameSpace <> sNameToTypeNameDoc n <+> var <+> "->"

enumFieldSizer :: Doc -> Field -> Doc
enumFieldSizer _ (EmptyField {}) = "Just 0"
enumFieldSizer fieldVar (Field {}) = "cautSize" <+> fieldVar

structFieldSizer :: Doc -> Doc -> Field -> Doc
structFieldSizer _ _ (EmptyField _ _) = "0"
structFieldSizer objName nameSpace (Field n _ _) =
  "cautSize" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

setFieldSizer :: Doc -> Doc -> Field -> Doc
setFieldSizer _ _ (EmptyField _ _) = "Just 0"
setFieldSizer objName nameSpace (Field n _ _) =
  "maybe (Just 0) cautSize" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

varSizeInsts :: Sized s => s -> Doc -> Doc
varSizeInsts sizeSpec sizeCheck =
  vcat [ sizeCheck, minSizeFromSize sizeSpec, maxSizeFromSize sizeSpec]
