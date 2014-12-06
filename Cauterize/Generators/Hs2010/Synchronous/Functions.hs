{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.Functions
  ( functions
  ) where

import Cauterize.FormHash
import Cauterize.Specification
import Text.PrettyPrint.Leijen.Text

import Cauterize.Generators.Hs2010.Synchronous.Common

functions :: Spec -> Doc
functions s = vcat (specHashFn s : hashLenFn : map typeHashFn (specTypes s))

hashLenFn :: Doc
hashLenFn = vcat [ "hashLen :: Int"
                 , "hashLen = length specHash"
                 ]

specHashFn :: Spec -> Doc
specHashFn s = vcat [ "specHash :: [Word8]"
                    , "specHash = " <> hashList
                    ]
  where
    hashList = hashToList $ specHash s

typeHashFn :: SpType -> Doc
typeHashFn t =
  let fn = sNameToVarNameDoc (typeName t) <> "Hash"
  in vcat [ fn <+> ":: [Word8]"
          , fn <+> "=" <+> hashToList (spHash t)
          ]

hashToList :: FormHash -> Doc
hashToList h = encloseSep "[ " " ]" ", " $ map (int . fromIntegral) $ hashToBytes h
