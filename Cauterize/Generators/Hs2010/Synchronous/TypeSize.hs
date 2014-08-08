{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Cauterize.Generators.Hs2010.Synchronous.TypeSize
  ( typeSizer
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M
import Data.Maybe

lkup :: Name -> M.Map Name SpType -> SpType
lkup n m = let e = error $ "MISTAKE: Unable to lookup name " ++ n ++ " in spec type map."
           in fromMaybe e $ n `M.lookup` m

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
typeSizer' _ (Scalar (TScalar n _) _ _) =
  let tnd = sNameToTypeNameDoc n
  in "cautSize (" <> tnd <> " x) = cautSize x"
typeSizer' _ (Const (TConst _ b _) _ _) =
  "cautSize _ = cautSize (undefined :: " <> biRepr b <> ")"
typeSizer' _ t@(Array (TArray _ _ l) _ s) =
  vcat [ "cautSize (" <> typeToTypeNameDoc t <> " v) = "
          <> align ("if " <> integer l <> " != V.length v"
            <$> indent 2 "then Nothing"
            <$> indent 2 "else liftM V.sum $ V.mapM cautSize v")
       , minSizeFromSize s
       , maxSizeFromSize s
       ]
typeSizer' _ t@(Vector (TVector _ _ l) _ s _) =
  vcat [ "cautSize (" <> typeToTypeNameDoc t <> " v) = "
          <> align ("if " <> integer l <> " < V.length v"
            <$> indent 2 "then Nothing"
            <$> indent 2 "else liftM V.sum $ V.mapM cautSize v")
       , minSizeFromSize s
       , maxSizeFromSize s
       ]
typeSizer' _ (Struct (TStruct n (Fields fs)) _ s) =
  let objName = "s"
      fsizes = punctuate " + " $ map (structFieldSizer objName $ sNameToVarNameDoc n) fs
  in vcat [ "cautSize" <+> objName <+> "=" <+> align (sep fsizes)
          , minSizeFromSize s
          , maxSizeFromSize s
          ]
typeSizer' _ (Set (TSet n (Fields fs)) _ s (FlagsRepr r)) =
  let objName = "s"
      repName = biRepr r
      fsizes = punctuate " + " $ map (setFieldSizer objName $ sNameToVarNameDoc n) fs
  in vcat [ "cautSize" <+> objName <+> "=" <+> align ("let repSize = cautSize (undefined :: " <> repName <> ")"
                                                  <$> "    fieldsSize = " <> align (sep fsizes)
                                                  <$> "in repSize + fieldsSize")
          , minSizeFromSize s
          , maxSizeFromSize s
          ]
typeSizer' _ (Enum (TEnum n (Fields fs)) _ s (TagRepr r)) =
  let objName = "e"
      fieldVar = "e'"
      repName = biRepr r
      enumCase f = let lhs = enumFieldCaseMatch fieldVar (sNameToTypeNameDoc n) f
                       rhs = enumFieldSizer fieldVar f
                   in lhs <+> rhs
      fcases = map enumCase fs
      caseBlob = align $ "e of" <$> vcat fcases
      lets = "let" <+> align ( "repSize = cautSize (undefined :: " <> repName <> ")"
                           <$> "fieldsSize = case " <> caseBlob)
      ins = "in repSize + fieldsSize"
  in vcat [ "cautSize" <+> objName <+> "=" <+> align (lets <$> ins)
          , minSizeFromSize s
          , maxSizeFromSize s
          ] 
typeSizer' _ (Pad (TPad _ l) _ _) =
  vcat [ "cautSize _ = " <> integer l
       ] 

enumFieldCaseMatch :: Doc -> Doc -> Field -> Doc
enumFieldCaseMatch _ nameSpace (EmptyField n _) = nameSpace <> sNameToTypeNameDoc n <+> "->"
enumFieldCaseMatch var nameSpace (Field n _ _) = nameSpace <> sNameToTypeNameDoc n <+> var <+> "->"

enumFieldSizer :: Doc -> Field -> Doc
enumFieldSizer _ (EmptyField {}) = "0"
enumFieldSizer fieldVar (Field {}) = "cautSize" <+> fieldVar

structFieldSizer :: Doc -> Doc -> Field -> Doc
structFieldSizer _ _ (EmptyField _ _) = "0"
structFieldSizer objName nameSpace (Field n _ _) =
  "cautSize (" <> nameSpace <> sNameToTypeNameDoc n <+> objName <> ")"

setFieldSizer :: Doc -> Doc -> Field -> Doc
setFieldSizer _ _ (EmptyField _ _) = "0"
setFieldSizer objName nameSpace (Field n _ _) =
  "maybe 0 cautSize (" <> nameSpace <> sNameToTypeNameDoc n <+> objName <> ")"
