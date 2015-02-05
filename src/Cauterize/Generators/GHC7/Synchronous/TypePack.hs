{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.TypePack
  ( typePacker
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.GHC7.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M

putFn :: Doc
putFn = "cautPut"

getFn :: Doc
getFn = "cautGet"

typePacker :: M.Map Name SpType -> SpType -> Doc
typePacker _ (BuiltIn {}) = empty
typePacker m t = let n = typeName t
                     inst = "instance CauterizeSerialize" <+> sNameToTypeNameDoc n <+> "where"
                              <$> indent 2 (align $ typePacker' m t <$> typeUnpacker' m t)
                 in inst <> linebreak

typePacker' :: M.Map Name SpType -> SpType -> Doc
typePacker' _ (BuiltIn {}) = error "Should never reach this."
typePacker' _ (Synonym s _ _) =
  let n = "x"
  in hsep [putFn, unpackSynonymAs s n, "=", putFn, n]
typePacker' _ (Array a _ _) =
  let n = "x"
      l = arrayLen a
      rest = ifStmt (integer l <+> "/= V.length" <+> n)
                    ("throwE \"Invalid Vector length for" <+> (sNameToTypeNameDoc . arrayName) a <+> ".\"")
                    ("V.mapM_" <+> putFn <+> n)
  in hsep [putFn, unpackArrayAs a n, "=", rest]
typePacker' _ (Vector v _ _ (LengthRepr lr)) =
  let n = "x"
      l = vectorMaxLen v
      rest = ifStmt (integer l <+> "< V.length" <+> n)
                    ("throwE \"Invalid Vector length for" <+> (sNameToTypeNameDoc . vectorName) v <+> ".\"")
                    ("do" <+> align (vcat [ putFn <+> parens ("fromIntegral $ V.length" <+> n `asType` biRepr lr)
                                          , "V.mapM_" <+> putFn <+> n
                                          ]))
  in hsep [putFn, unpackVectorAs v n, "=", rest]
typePacker' _ (Record (TRecord n (Fields fs)) _ _) =
  let objName = "s"
      fputers = map (recordFieldPuter objName $ sNameToVarNameDoc n) fs
  in putFn <+> objName <+> "=" <+> "do" <$> indent 2 (vcat fputers)
typePacker' _ (Combination (TCombination n (Fields fs)) _ _ (FlagsRepr r)) =
  let objName = "s"
      flagExps = encloseSep "[ " " ]" ", " $ map (combinationFieldFlager objName $ sNameToVarNameDoc n) fs
      lete = "let flags = boolsToBits" <+> flagExps <+> "::" <+> biRepr r
      fputers = putFn <+> "flags" : map (combinationFieldPuter objName $ sNameToVarNameDoc n) fs
      ine = "in do" <+> align (vcat fputers)
  in putFn <+> objName <+> "=" <+> align (lete <$> ine)
typePacker' _ t@(Union (TUnion _ (Fields fs)) _ _ (TagRepr tr)) =
  let tnd = typeToTypeNameDoc t
  in vcat $ map (unionFieldPacker putFn tnd tr) fs

typeUnpacker' :: M.Map Name SpType -> SpType -> Doc
typeUnpacker' _ (BuiltIn {}) = error "Should never reach this."
typeUnpacker' _ (Synonym (TSynonym n _) _ _) =
  let n' = sNameToTypeNameDoc n
  in hsep [getFn, "=", "liftM", n', "cautGet"]
typeUnpacker' _ (Array (TArray n _ l) _ _) =
  let n' = sNameToTypeNameDoc n
  in getFn <+> "=" <+> unpackArrayOfLen (integer l) n'
typeUnpacker' _ (Vector (TVector n _ l) _ _ (LengthRepr lr)) =
  let n' = sNameToTypeNameDoc n
      aligned = align $ vcat [ "len <-" <+> getFn `asType` ("ExceptT String S.Get" <+> biRepr lr)
                             , "if len >" <+> integer l
                             , "  then throwE $ \"Invalid length unpacked: \" ++ show len"
                             , "  else" <+> unpackArrayOfLen "(fromIntegral len)" n'
                             ]
  in getFn <+> "= do" <+> aligned
typeUnpacker' _ (Record (TRecord n (Fields fs)) _ _) =
  let fNames = nameFields fs
      n' = sNameToTypeNameDoc n
  in getFn <+> "= do" <+> align ( vcat (map recordFieldGetter fNames)
                              <$> "return" <+> parens (n' <+> hsep fNames)
                                )
typeUnpacker' _ (Combination (TCombination n (Fields fs)) _ _ (FlagsRepr fr)) =
  let fNames = take (length fs) manyNames
      n' = sNameToTypeNameDoc n
  in getFn <+> "= do" <+> align ( "flags <- cautGet :: ExceptT String S.Get" <+> biRepr fr
                              <$> "if zeroBits /=" <+> parens ("Bits.complement" <+> int (((2 :: Int) ^ length fs) - 1) <+> ".&. flags")
                              <$> "  then throwE $ \"Flags out of range. Flags were: \" ++ show flags"
                              <$> "  else do" <+> ( align ( vcat (map combinationFieldGetter (zip fs fNames)) <$> "return" <+> parens (n' <+> hsep fNames))))
typeUnpacker' _ t@(Union (TUnion _ (Fields fs)) _ _ (TagRepr tr)) =
  let tnd = typeToTypeNameDoc t
  in getFn <+> " = do" <+> (align $ vcat [ "tag <- cautGet" `asType` "ExceptT String S.Get" <+> biRepr tr
                            , "case tag of"
                            ] <$> (indent 2 $ (vcat $ map (unionFieldUnpacker tnd) fs)
                                          <$> "_ -> throwE $ \"Invalid tag: \" ++ show tag"))

unpackArrayOfLen :: Doc -> Doc -> Doc
unpackArrayOfLen len constructor = "liftM" <+> constructor <+> "$ V.sequence" <+> parens ("V.fromList $ replicate" <+> len <+> "cautGet")

recordFieldPuter :: Doc -> Doc -> Field -> Doc
recordFieldPuter _ _ (EmptyField {}) = empty
recordFieldPuter objName nameSpace (Field n _ _) = putFn <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

recordFieldGetter :: Doc -> Doc
recordFieldGetter n = n <+> "<- cautGet"

combinationFieldPuter :: Doc -> Doc -> Field -> Doc
combinationFieldPuter _ _ (EmptyField {}) = empty
combinationFieldPuter objName nameSpace (Field n _ _) = "putIfJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

combinationFieldFlager :: Doc -> Doc -> Field -> Doc
combinationFieldFlager objName nameSpace f =
  let n = fName f
  in "isJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

combinationFieldGetter :: (Field, Doc) -> Doc
combinationFieldGetter (f, d) = hsep [d, "<- if", check, "then", ifThen, "else", ifElse]
  where
    i = fIndex f
    check = "flags `testBit`" <+> integer i
    ifThen = case f of
              EmptyField {} -> "return (Just ())"
              Field {} -> "cautGet >>= return . Just"
    ifElse = "return Nothing"

unionFieldPacker :: Doc -> Doc -> BuiltIn -> Field -> Doc
unionFieldPacker func prefix tr f =
  let fn = prefix <> sNameToTypeNameDoc (fName f)
      pkTag = putFn <+> parens (integer (fIndex f) `asType` biRepr tr)
      containedName = "a"
  in case f of
      EmptyField {} -> func <+> fn <+> "=" <+> pkTag
      Field {} -> func <+> parens (fn <+> containedName) <+> "=" <+> pkTag <+> ">>" <+> putFn <+> containedName

unionFieldUnpacker :: Doc -> Field -> Doc
unionFieldUnpacker prefix f =
  let fn = prefix <> sNameToTypeNameDoc (fName f)
      tagMatch = integer (fIndex f) <+> "->"
  in tagMatch <+> case f of
                    EmptyField {} -> "return" <+> fn
                    Field {} -> "liftM" <+> fn <+> "cautGet"
