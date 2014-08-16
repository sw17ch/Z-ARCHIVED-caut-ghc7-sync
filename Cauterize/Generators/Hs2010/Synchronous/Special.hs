{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.Special
  ( arbInst
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text

arbFn :: Doc
arbFn = "arbitrary"

qArbFn :: Doc
qArbFn = "QC." <> arbFn

arbInst :: SpType -> Doc
arbInst (BuiltIn {}) = empty
arbInst t = let n = typeName t
                inst = "instance QC.Arbitrary" <+> sNameToTypeNameDoc n <+> "where"
                       <$> indent 2 (arbInst' t)
            in inst <> linebreak
arbInst' :: SpType -> Doc
arbInst' (BuiltIn {}) = error "Should not ever reach this."
arbInst' t@(Scalar {}) = 
  let tnd = typeToTypeNameDoc t
  in arbFn <+> "= liftM" <+> tnd <+> qArbFn
arbInst' t@(Const {}) =
  let tnd = typeToTypeNameDoc t
  in arbFn <+> "= return" <+> tnd
arbInst' t@(Array (TArray _ _ al) _ _) =
  let tnd = typeToTypeNameDoc t
  in arbFn <+> "= do" <+> (align (vcat [ "es <- QC.vectorOf" <+> integer al <+> qArbFn
                                       , "return $" <+> tnd <+> "(V.fromList es)"
                                       ]))
arbInst' t@(Vector (TVector _ _ ml) _ _ _) =
  let tnd = typeToTypeNameDoc t
  in arbFn <+> "= do" <+> (align (vcat [ "vl <- QC.choose (0," <> integer ml <> ")"
                                       , "es <- QC.vectorOf vl" <+> qArbFn
                                       , "return $" <+> tnd <+> "(V.fromList es)"
                                       ]))
arbInst' t@(Struct (TStruct _ (Fields fs)) _ _) =
  let tnd = typeToTypeNameDoc t
      arbNs = nameFields fs
      arbs = map (\a -> a <+> "<-" <+> qArbFn) arbNs
  in arbFn <+> "= do" <+> (align $ (vcat arbs) <$> ("return" <+> parens (tnd <+> hsep arbNs)))
arbInst' t@(Set (TSet _ (Fields fs)) _ _ _) =
  let tnd = typeToTypeNameDoc t
      arbNs = take (length fs) manyNames
      arbs = map (\a -> a <+> "<-" <+> qArbFn) arbNs
  in arbFn <+> "= do" <+> (align $ (vcat arbs) <$> ("return" <+> parens (tnd <+> hsep arbNs)))
arbInst' t@(Enum (TEnum _ (Fields fs)) _ _ _) =
  let arbFieldFns = map (arbEnumField $ typeToTypeNameDoc t) fs
  in arbFn <+> "= QC.oneof" <+> encloseSep "[ " (line <> "]") ", " arbFieldFns
  
arbInst' t@(Pad {}) =
  let tnd = typeToTypeNameDoc t
  in arbFn <+> "= return" <+> tnd

arbEnumField :: Doc -> Field -> Doc
arbEnumField prefix (EmptyField n _) = "return" <+> prefix <> sNameToTypeNameDoc n
arbEnumField prefix (Field n _ _) = "liftM" <+> prefix <> sNameToTypeNameDoc n <+> qArbFn
