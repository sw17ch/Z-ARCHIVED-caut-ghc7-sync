{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.TypePack
  ( typePacker
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe

putFn :: Doc
putFn = "cautPut"

getFn :: Doc
getFn = "cautGet"

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
typePacker' _ (Const c _ _) = hsep [putFn, "_ =", putFn, constAsRepr c]
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
typePacker' _ (Struct (TStruct n (Fields fs)) _ _) =
  let objName = "s"
      fputers = map (structFieldPuter objName $ sNameToVarNameDoc n) fs
  in putFn <+> objName <+> "=" <+> "do" <$> indent 2 (vcat fputers)
typePacker' _ (Set (TSet n (Fields fs)) _ _ (FlagsRepr r)) =
  let objName = "s"
      flagExps = encloseSep "[ " " ]" ", " $ map (setFieldFlager objName $ sNameToVarNameDoc n) fs
      lete = "let flags = boolsToBits" <+> flagExps <+> "::" <+> biRepr r
      fputers = putFn <+> "flags" : map (setFieldPuter objName $ sNameToVarNameDoc n) fs
      ine = "in do" <+> align (vcat fputers)
  in putFn <+> objName <+> "=" <+> align (lete <$> ine)
typePacker' _ t@(Enum (TEnum _ (Fields fs)) _ _ (TagRepr tr)) =
  let tnd = typeToTypeNameDoc t
  in vcat $ map (enumFieldPacker putFn tnd tr) fs
typePacker' _ (Pad (TPad _ ln) _ _) =
  putFn <+> "_ =" <+> align (vcat ["let bsPad = B.pack $ replicate" <+> integer ln <+> "(0 :: U8)"
                                  ,"in lift $ P.putByteString bsPad"
                                  ])

typeUnpacker' :: M.Map Name SpType -> SpType -> Doc
typeUnpacker' _ (BuiltIn {}) = error "Should never reach this."
typeUnpacker' _ (Scalar (TScalar n _) _ _) =
  let n' = sNameToTypeNameDoc n
  in hsep [getFn, "=", "liftM", n', "cautGet"]
typeUnpacker' _ (Const (TConst n r v ) _ _) =
  let r' = case r of
            BIbool -> biRepr BIu8
            _ -> biRepr r
  in getFn <+> "= do" <+> align (vcat [ "v <- cautGet :: ExceptT String S.Get" <+> r'
                                      , "if" <+> integer v <+> "== v"
                                      , indent 2 "then return " <+> sNameToTypeNameDoc n
                                      , indent 2 $ "else throwE $ \"Invalid constant value. Expected" <+> integer v <> ". Got: \" ++ show v"

                                      ])
typeUnpacker' _ (Array (TArray n _ l) _ _) =
  let n' = sNameToTypeNameDoc n
  in getFn <+> "=" <+> unpackArrayOfLen (integer l) n'
typeUnpacker' _ (Vector (TVector n _ l) _ _ (LengthRepr lr)) =
  let n' = sNameToTypeNameDoc n
  in getFn <+> "= do" <+> (align $ vcat [ "len <-" <+> getFn `asType` ("ExceptT String S.Get" <+> biRepr lr)
                                        , "if len >" <+> integer l
                                        , "  then throwE $ \"Invalid length unpacked: \" ++ show len"
                                        , "  else" <+> unpackArrayOfLen "(fromIntegral len)" n'
                                        ])
typeUnpacker' _ (Struct (TStruct n (Fields fs)) _ _) =
  let fNames = nameFields fs
      n' = sNameToTypeNameDoc n
  in getFn <+> "= do" <+> align ( vcat (map structFieldGetter fNames)
                              <$> "return" <+> parens (n' <+> hsep fNames)
                                )
typeUnpacker' _ (Set (TSet n (Fields fs)) _ _ (FlagsRepr fr)) =
  let fNames = take (length fs) manyNames
      n' = sNameToTypeNameDoc n
  in getFn <+> "= do" <+> align ( "flags <- cautGet :: ExceptT String S.Get" <+> biRepr fr
                              <$> "if zeroBits /=" <+> parens ("Bits.complement" <+> int (((2 :: Int) ^ (length fs)) - 1) <+> ".&. flags")
                              <$> "  then throwE $ \"Flags out of range. Flags were: \" ++ show flags"
                              <$> "  else do" <+> ( align ( vcat (map setFieldGetter (zip fs fNames))
                                                        <$> "return" <+> parens (n' <+> hsep fNames))))
typeUnpacker' _ t@(Enum (TEnum _ (Fields fs)) _ _ (TagRepr tr)) =
  let tnd = typeToTypeNameDoc t
  in getFn <+> " = do" <+> (align $ vcat [ "tag <- cautGet" `asType` "ExceptT String S.Get" <+> biRepr tr
                            , "case tag of"
                            ] <$> (indent 2 $ (vcat $ map (enumFieldUnpacker tnd) fs)
                                          <$> "_ -> throwE $ \"Invalid tag: \" ++ show tag"))
typeUnpacker' _ (Pad (TPad n ln) _ _) =
  let n' = sNameToTypeNameDoc n
  in getFn <+> "="
     <+> "do" <+> align (vcat [ "bsPad <- lift $ S.getBytes" <+> integer ln
                              , "if all (== 0) (B.unpack bsPad)"
                              , "  then return" <+> n'
                              , "  else throwE \"The " <+> integer ln <+> "padding bytes were not all NULL.\""
                              ])

unpackArrayOfLen :: Doc -> Doc -> Doc
unpackArrayOfLen len constructor = "liftM" <+> constructor <+> "$ V.sequence" <+> parens ("V.fromList $ replicate" <+> len <+> "cautGet")

structFieldPuter :: Doc -> Doc -> Field -> Doc
structFieldPuter _ _ (EmptyField {}) = empty
structFieldPuter objName nameSpace (Field n _ _) = putFn <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

structFieldGetter :: Doc -> Doc
structFieldGetter n = n <+> "<- cautGet"

setFieldPuter :: Doc -> Doc -> Field -> Doc
setFieldPuter _ _ (EmptyField {}) = empty
setFieldPuter objName nameSpace (Field n _ _) = "putIfJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

setFieldFlager :: Doc -> Doc -> Field -> Doc
setFieldFlager objName nameSpace f = let n = fName f
                                     in "isJust" <+> parens (nameSpace <> sNameToTypeNameDoc n <+> objName)

setFieldGetter :: (Field, Doc) -> Doc
setFieldGetter (f, d) = hsep [d, "<- if", check, "then", ifThen, "else", ifElse]
  where
    i = fIndex f
    check = "flags `testBit`" <+> integer i
    ifThen = case f of
              EmptyField {} -> "return (Just ())"
              Field {} -> "cautGet >>= return . Just"
    ifElse = "return Nothing"

enumFieldPacker :: Doc -> Doc -> BuiltIn -> Field -> Doc
enumFieldPacker func prefix tr f =
  let fn = prefix <> sNameToTypeNameDoc (fName f)
      pkTag = putFn <+> parens (integer (fIndex f) `asType` biRepr tr)
      containedName = "a"
  in case f of
      EmptyField {} -> func <+> fn <+> "=" <+> pkTag
      Field {} -> func <+> parens (fn <+> containedName) <+> "=" <+> pkTag <+> ">>" <+> putFn <+> containedName

enumFieldUnpacker :: Doc -> Field -> Doc
enumFieldUnpacker prefix f =
  let fn = prefix <> sNameToTypeNameDoc (fName f)
      tagMatch = integer (fIndex f) <+> "->"
  in tagMatch <+> case f of
                    EmptyField {} -> "return" <+> fn
                    Field {} -> "liftM" <+> fn <+> "cautGet"
      
