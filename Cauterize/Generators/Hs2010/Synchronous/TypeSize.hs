{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.TypeSize
  ( typeSizer
  ) where

import Cauterize.Specification
import Cauterize.Common.Types
import Cauterize.Generators.Hs2010.Synchronous.Common

import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe

lkup :: Name -> M.Map Name SpType -> SpType
lkup n m = let e = error $ "MISTAKE: Unable to lookup name " ++ n ++ " in spec type map."
           in fromMaybe e $ n `M.lookup` m

typeSizer :: M.Map Name SpType -> SpType -> Doc
typeSizer _ (BuiltIn {}) = empty
typeSizer m t = let n = typeName t
                in "instance CauterizeSize" <+> sNameToTypeNameDoc n <+> "where" <$> indent 2 (typeSizer' m t)
                 
typeSizer' :: M.Map Name SpType -> SpType -> Doc
typeSizer' _ (BuiltIn {}) = error "Should never reach this."
typeSizer' _ (Scalar (TScalar n _) _ _) =
  let tnd = sNameToTypeNameDoc n
  in "cautSize (" <> tnd <> " x) = cautSize x"
typeSizer' _ (Const (TConst _ b _) _ _) =
  "cautSize _ = cautSize (undefined :: " <> biRepr b <> ")"
typeSizer' tm (Array (TArray _ b _) _ _) =
  "cautSize _ = cautSize (undefined :: " <> typeToTypeNameDoc (b `lkup` tm) <> ")"
typeSizer' _ _ = "????????????????????????????????????"
          
{-
typeSizer :: SpType -> Doc
typeSizer (BuiltIn {}) = empty
typeSizer (Scalar (TScalar n _) _ s) = staticSize (T.pack n) s
typeSizer (Const (TConst n _ _) _ s) = staticSize (T.pack n) s
typeSizer (Array (TArray n _ _) _ _) =
  sizerInstance n (parens (sNameToTypeNameDoc n <+> "vec")) " = (V.sum . V.map typeSizer) vec"
typeSizer (Vector (TVector n _ _) _ _ _) =
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
typeSizer (Pad (TPad n l) _ _) = sizerInstance n "_" $ " = " <> integer l

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
       -}
