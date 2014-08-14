{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.TypePack
  ( typePacker
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M

putFn :: Doc
putFn = "cautPut"

typePacker :: M.Map Name SpType -> SpType -> Doc
typePacker _ (BuiltIn {}) = empty
typePacker m t = let n = typeName t
                     inst = "instance CauterizeSerialize" <+> sNameToTypeNameDoc n <+> "where"
                              <$> indent 2 (align $ (typePacker' m t) <$> (typeUnpacker' m t))
                 in inst <> linebreak

typePacker' :: M.Map Name SpType -> SpType -> Doc
typePacker' _ (BuiltIn {}) = error "Should never reach this."
typePacker' _ (Scalar s _ _) =
  let n = "x"
  in hsep [putFn, unpackScalarAs s n, "=", putFn, n]
typePacker' _ (Const c _ _) = hsep [putFn, "_ =", putFn, constAsUndefined c]
typePacker' _ (Array a _ _) =
  let n = "x"
      l = arrayLen a
      rest = ifStmt (integer l <+> "== V.length" <+> n)
                    ("return $ Left \"Invalid Vector length for" <+> (sNameToTypeNameDoc . arrayName) a <+> ".\"")
                    ("liftM V.sequence_ $ V.mapM" <+> putFn <+> n)
  in hsep [putFn, unpackArrayAs a n, "=", rest]
typePacker' _ (Vector v _ _ (LengthRepr lr)) =
  let n = "x"
      l = vectorMaxLen v
      rest = ifStmt (integer l <+> "< V.length" <+> n)
                    ("return $ Left \"Invalid Vector length for" <+> (sNameToTypeNameDoc . vectorName) v <+> ".\"")
                    ("do" <+> align (vcat [ putFn <+> parens ("fromIntegral $ V.length" <+> n `asType` biRepr lr)
                                          , "liftM V.sequence_ $ V.mapM" <+> putFn <+> n
                                          ]))
  in hsep [putFn, unpackVectorAs v n, "=", rest]
typePacker' _ (Struct (TStruct n (Fields fs)) _ _) =
  let objName = "s"
      fputers = map (structFieldPuter objName $ sNameToVarNameDoc n) fs
  in putFn <+> objName <+> "=" <+> "do" <$> indent 2 (vcat fputers)
typePacker' _ (Set (TSet n (Fields fs)) _ s (FlagsRepr r)) =
  let objName = "s"
      flagExps = encloseSep "[ " (line <> "]") ", " $ map (setFieldFlager objName $ sNameToVarNameDoc n) fs
      lete = "let flags = boolsToBits" <+> flagExps <+> "::" <+> biRepr r
      fputers = "cautPut flags" : map (setFieldPuter objName $ sNameToVarNameDoc n) fs
      ine = "in do" <+> align (vcat fputers)
  in putFn <+> objName <+> "=" <+> align (lete <$> ine)
  
typePacker' _ _ = empty

typeUnpacker' :: M.Map Name SpType -> SpType -> Doc
typeUnpacker' _ _ = empty

structFieldPuter :: Doc -> Doc -> Field -> Doc
structFieldPuter _ _ (EmptyField {}) = empty
structFieldPuter objName nameSpace (Field n _ _) = putFn <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

setFieldPuter :: Doc -> Doc -> Field -> Doc
setFieldPuter _ _ (EmptyField {}) = empty
setFieldPuter objName nameSpace (Field n _ _) = "putIfJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

setFieldFlager :: Doc -> Doc -> Field -> Doc
setFieldFlager objName nameSpace f = let n = fName f
                                     in "isJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)
