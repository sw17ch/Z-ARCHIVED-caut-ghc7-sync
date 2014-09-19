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
    -- tm = specTypeMap spec
    n = T.pack $ specName spec
    r = renderPretty 0.4 80
    ts = AI.aiTypes ai
    aiTypeName = (text . nameToCapHsName) n <> "AI"
    parts = vcat [ imports
                 , linebreak
                 , typeDecl
                 , linebreak
                 , typeTags
                 , linebreak
                 , typeSize
                 , linebreak
                 , typeSerialize
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <> "AI" <+> "where"

    imports = vcat [ "import Cauterize." <> (text . nameToCapHsName $ n)
                   ]

    typeDecl = "data " <> aiTypeName <> typeAlts
                       <> linebreak <> (indent 2 "deriving (Show, Eq, Ord)")
    typeAlts = encloseSep " = " empty " | " $ map mkAlt ts

    typeTags = vcat $ map mkTag ts

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

    typeSerialize = let d = "aiPack" <> aiTypeName
                        t = d <+> "::" <+> aiTypeName <+> "-> B.ByteString"
                        i = d <+> "t" <+> "=" <+> "undefined"
                    in t <$> i <$> (indent 2 "where")

    typeGet = "cautGet = ???"
    typePut = "cautPut t = case t of" <$> indent 14 putters

    putters = vcat $ map mkPutter ts

mkTag :: AI.AiType -> Doc
mkTag (AI.AiType n p) =
  let n' = "tag" <> (text . nameToCapHsName . T.pack) n
  in vcat [ n' <+> ":: [U8]"
          , n' <+> "= [" <+> (hcat $ punctuate  "," (map (text . T.pack . show) p)) <+> "]"
          ]

hsTypeName :: AI.AiType -> Doc
hsTypeName t = (text . nameToCapHsName . T.pack . AI.aiTypeName) t

msgName :: AI.AiType -> Doc
msgName t = "Msg" <> hsTypeName t

mkPutter :: AI.AiType -> Doc
mkPutter t = msgName t <+> "->" <+> rhs
  where
    rhs = vcat [ "mapM_ cautPut tag" <> hsTypeName t
               ]
           
mkAlt :: AI.AiType -> Doc
mkAlt t = msgName t <+> (text . nameToCapHsName . T.pack . AI.aiTypeName) t

mkAltSize :: AI.AiType -> Doc
mkAltSize t = msgName t <+> "t -> cautSize t"

mkAltMinSize :: AI.AiType -> Doc
mkAltMinSize t = msgName t <+> "t -> minSize t"

mkAltMaxSize :: AI.AiType -> Doc
mkAltMaxSize t = msgName t <+> "t -> maxSize t"
