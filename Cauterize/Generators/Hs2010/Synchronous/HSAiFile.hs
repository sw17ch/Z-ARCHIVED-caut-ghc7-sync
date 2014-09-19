{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.Hs2010.Synchronous.HSAiFile
  ( hsAiFileName
  , renderAIFile
  ) where

import Cauterize.Specification
import qualified Cauterize.AI as AI

import Cauterize.Generators.Hs2010.Synchronous.Common

import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

renderAIFile :: Spec -> AI.Ai -> T.Text
renderAIFile spec ai = displayT . r $ hsMod <> linebreak <$> parts
  where
    tm = specTypeMap spec
    n = T.pack $ specName spec
    r = renderPretty 0.4 80
    ts = specTypes spec
    aiTypeName = (text . nameToCapHsName) n <> "AI"
    parts = vcat [ imports
                 , linebreak
                 , typeDecl
                 , linebreak
                 , typeSize
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <> "AI" <+> "where"

    imports = vcat [ "import Cauterize." <> (text . nameToCapHsName $ n)
                   ]

    typeDecl = "data " <> aiTypeName <+> typeAlts
                       <> linebreak <> (indent 2 "deriving (Show, Eq, Ord)")
    typeAlts = encloseSep " = " empty " | " $ map mkAlt ts
    mkAlt t = let n' = text . nameToCapHsName . T.pack $ typeName t
              in "Msg" <> n' <+> n'

    mkAltSize t = let n' = text . nameToCapHsName . T.pack $ typeName t
                  in "Msg" <> n' <+> "t -> cautSize t"
    mkAltMinSize t = let n' = text . nameToCapHsName . T.pack $ typeName t
                     in "Msg" <> n' <+> "t -> minSize t"
    mkAltMaxSize t = let n' = text . nameToCapHsName . T.pack $ typeName t
                     in "Msg" <> n' <+> "t -> maxSize t"

    typeSize = let i = "instance CauterizeSize" <+> aiTypeName <+> "where"
                   overhead = integer $ (AI.aiTypeLength ai) + (AI.aiDataLength ai)
                   cs = indent 2 $ vcat $
                          ( "cautSize t =" <+> overhead <+> "+ case t of"
                          : (map ((indent 19) . mkAltSize) ts))
                   mis = indent 2 $ vcat $
                          ( "minSize t =" <+> overhead <+> "+ case t of"
                          : (map ((indent 19) . mkAltMinSize) ts))
                   mas = indent 2 $ vcat $
                          ( "maxSize t =" <+> overhead <+> "+ case t of"
                          : (map ((indent 19) . mkAltMaxSize) ts))
               in i <$> cs <$> mis <$> mas
      
{-
, text $ concat [ "#define MESSAGE_OVERHEAD_" ++ libName ++ "("
                , (show . AI.aiTypeLength) ai
                , " + "
                , (show . AI.aiDataLength) ai
                , ")"
                ]
                -}
