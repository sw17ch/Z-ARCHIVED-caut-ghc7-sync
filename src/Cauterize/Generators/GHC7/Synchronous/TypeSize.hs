{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.TypeSize
  ( typeSizer
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.GHC7.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M
import Data.Maybe

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
typeSizer' _ (Synonym s _ _) = hsep [sizeFn, unpackSynonymAs s "x", "=", sizeFn, "x"]
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
typeSizer' _ (Record (TRecord n (Fields fs)) _ s) =
  let objName = "s"
      fsizes = mapMaybe (recordFieldSizer objName $ sNameToVarNameDoc n) fs
      doblk = "liftM sum $ sequence " <+> align (encloseSep "[ " (line <> "]") ", " fsizes)
  in varSizeInsts s $ "cautSize" <+> objName <+> "=" <+> doblk
typeSizer' _ (Combination (TCombination n (Fields fs)) _ s (FlagsRepr r)) =
  let objName = "s"
      repName = biRepr r
      fsizes = mapMaybe (combinationFieldSizer objName $ sNameToVarNameDoc n) fs
  in varSizeInsts s $
    "cautSize" <+> objName <+> "=" <+>
      "do" <+> align (vcat [ "flagsSize <- cautSize (undefined :: " <> repName <> ")"
                           , "fieldsSize <- liftM sum $ sequence" <+>
                              align (encloseSep "[ " (line <> "]") ", " fsizes)
                           , "return $ flagsSize + fieldsSize"
                           ])
typeSizer' _ (Union (TUnion n (Fields fs)) _ s (TagRepr r)) =
  let objName = "e"
      fieldVar = "e'"
      repName = biRepr r
      enumCase f = let lhs = unionFieldCaseMatch fieldVar (sNameToTypeNameDoc n) f
                       rhs = unionFieldSizer fieldVar f
                   in lhs <+> rhs
      fcases = map enumCase fs
      caseBlob = align $ "e of" <$> vcat fcases
      e = "do" <+> align ( "repSize <- cautSize (undefined :: " <> repName <> ")"
                       <$> "fieldsSize <- case " <> align caseBlob
                       <$> "return $ repSize + fieldsSize")
  in varSizeInsts s $ "cautSize" <+> objName <+> "=" <+> e

unionFieldCaseMatch :: Doc -> Doc -> Field -> Doc
unionFieldCaseMatch _ nameSpace (EmptyField n _) = nameSpace <> sNameToTypeNameDoc n <+> "->"
unionFieldCaseMatch var nameSpace (Field n _ _) = nameSpace <> sNameToTypeNameDoc n <+> var <+> "->"

unionFieldSizer :: Doc -> Field -> Doc
unionFieldSizer _ (EmptyField {}) = "Just 0"
unionFieldSizer fieldVar (Field {}) = "cautSize" <+> fieldVar

recordFieldSizer :: Doc -> Doc -> Field -> Maybe Doc
recordFieldSizer _ _ (EmptyField _ _) = Nothing
recordFieldSizer objName nameSpace (Field n _ _) = Just $
  "cautSize" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

combinationFieldSizer :: Doc -> Doc -> Field -> Maybe Doc
combinationFieldSizer _ _ (EmptyField _ _) = Nothing
combinationFieldSizer objName nameSpace f = Just $
  "maybe (Just 0) cautSize" <+> parens (nameSpace <> sNameToTypeNameDoc (fName f) <+> objName)

varSizeInsts :: Sized s => s -> Doc -> Doc
varSizeInsts sizeSpec sizeCheck =
  vcat [ sizeCheck, minSizeFromSize sizeSpec, maxSizeFromSize sizeSpec]
