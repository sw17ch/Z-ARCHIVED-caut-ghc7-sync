{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Cauterize.Generators.GHC7.Synchronous.Common
  ( sNameToTypeNameDoc
  , sNameToVarNameDoc
  , typeToTypeNameDoc
  , typeToVarNameDoc
  , hsFileName
  , hsMetaFileName
  , hsTsFileName
  , libName
  , biRepr
  , biReprText
  , nameToCapHsName

  , spacedBraces
  , fieldDecl
  , asType
  , ifStmt
  , lkup
  , manyNames
  , nameFields

  , unpackSynonymAs
  , builtinAsUndefined
  , tyNameAsUndefined
  , unpackArrayAs
  , unpackVectorAs
  ) where

import Cauterize.Specification
import Cauterize.Common.Types

import qualified Data.Text.Lazy as T
import qualified Data.Char as C
import qualified Data.Map as M
import Text.PrettyPrint.Leijen.Text
import Data.Maybe

libName :: Spec -> T.Text
libName = specName

hsFileName :: Spec -> FilePath
hsFileName s = let part = nameToCapHsName $ specName s
                   suff = ".hs"
               in T.unpack $ part `T.append` suff

hsMetaFileName :: Spec -> FilePath
hsMetaFileName s = let part = nameToCapHsName $ specName s
                       suff = "Meta.hs"
                   in T.unpack $ part `T.append` suff

hsTsFileName :: Spec -> FilePath
hsTsFileName _ = "test_server.hs"

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

sNameToTypeNameDoc :: T.Text -> Doc
sNameToTypeNameDoc = text . nameToCapHsName

sNameToVarNameDoc :: T.Text -> Doc
sNameToVarNameDoc = text . nameToHsName

typeToTypeNameDoc :: SpType -> Doc
typeToTypeNameDoc = sNameToTypeNameDoc . typeName

typeToVarNameDoc :: SpType -> Doc
typeToVarNameDoc = sNameToVarNameDoc . typeName

biRepr :: BuiltIn -> Doc
biRepr = text . biReprText

biReprText :: BuiltIn -> T.Text
biReprText BIu8 = "U8"
biReprText BIu16 = "U16"
biReprText BIu32 = "U32"
biReprText BIu64 = "U64"
biReprText BIs8 = "S8"
biReprText BIs16 = "S16"
biReprText BIs32 = "S32"
biReprText BIs64 = "S64"
biReprText BIf32 = "F32"
biReprText BIf64 = "F64"
biReprText BIbool = "Bool"

ifStmt :: Doc -> Doc -> Doc -> Doc
ifStmt cond t e = "if" <> align (vcat [" " <> cond, "then" <+> t, "else" <+> e])

spacedBraces :: Doc -> Doc
spacedBraces p = braces $ " " <> p <> " "

-- TODO: Should we attempt to drop some annotation in the Haskell code that an
-- empty field was left out?
fieldDecl :: Doc -> Field -> Maybe Doc
fieldDecl _ (EmptyField _ _) = Nothing
fieldDecl nameSpace (Field fn fr _) =
  let fn' = nameSpace <> sNameToTypeNameDoc fn
      fr' = sNameToTypeNameDoc fr
  in Just $ fn' `asType` fr'

asType :: Doc -> Doc -> Doc
asType lhs rhs = lhs <+> "::" <+> rhs

unpackSynonymAs :: TSynonym -> Doc -> Doc
unpackSynonymAs (TSynonym n _) a =
  let tnd = sNameToTypeNameDoc n
  in parens $ tnd <+> a

builtinAsUndefined :: BuiltIn -> Doc
builtinAsUndefined b = tyNameAsUndefined (biRepr b)

tyNameAsUndefined :: Doc -> Doc
tyNameAsUndefined n = parens $ "undefined" `asType` n

unpackArrayAs :: TArray -> Doc -> Doc
unpackArrayAs (TArray n _ _) a =
  let tnd = sNameToTypeNameDoc n
  in parens $ tnd <+> a

unpackVectorAs :: TVector -> Doc -> Doc
unpackVectorAs (TVector n _ _) a =
  let tnd = sNameToTypeNameDoc n
  in parens $ tnd <+> a

lkup :: Name -> M.Map Name SpType -> SpType
lkup n m = let e = error $ "MISTAKE: Unable to lookup name " ++ T.unpack n ++ " in spec type map."
           in fromMaybe e $ n `M.lookup` m

manyNames :: [Doc]
manyNames = map (\i -> text $ T.pack $ 'f':show i) ([0..] :: [Integer])

nameFields :: [Field] -> [Doc]
nameFields fs = mapMaybe go (zip fs manyNames)
  where
    go (EmptyField {}, _) = Nothing
    go (Field {}, n) = Just n
