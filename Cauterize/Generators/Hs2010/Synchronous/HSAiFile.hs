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
                 , utils
                 , linebreak
                 , typeSerialize
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <> "AI" <+> "where"

    imports = vcat [ "import Cauterize." <> (text . nameToCapHsName $ n)
                   , "import Cauterize.Support.Hs2010"
                   , "import qualified Data.ByteString as B"
                   ]

    typeDecl = "data " <> aiTypeName <> typeAlts
                       <> linebreak <> (indent 2 "deriving (Show, Eq, Ord)")
    typeAlts = encloseSep " = " empty " | " $ map mkAlt ts

    typeTags = vcat $ map mkTag ts

    typeSize = let i = "instance CauterizeSize" <+> aiTypeName <+> "where"
                   overhead = integer $ (AI.aiTypeLength ai) + (AI.aiDataLength ai)
                   cs = indent 2 $ vcat $
                          ( "cautSize t = fmap (" <> overhead <+> "+) $ case t of"
                          : (map ((indent 29) . mkAltSize) ts))
                   mis = indent 2 $ vcat $
                          ( "minSize t =" <+> overhead <+> "+ case t of"
                          : (map ((indent 19) . mkAltMinSize) ts))
                   mas = indent 2 $ vcat $
                          ( "maxSize t =" <+> overhead <+> "+ case t of"
                          : (map ((indent 19) . mkAltMaxSize) ts))
               in i <$> cs <$> mis <$> mas

    typeSerialize = let d = "aiPack" <> aiTypeName
                        t = d <+> "::" <+> aiTypeName <+> "-> Either String B.ByteString"
                        mk (AI.AiType ain _) =
                          let ain' = (text . nameToCapHsName . T.pack) ain
                          in d <+> parens ("Msg" <> ain' <+> "t") <+> "=" <+> (mkBody ain')
                        is = vcat $ map mk ts
                        mkBody ain =
                          align $ vcat ["do" <+>
                                          (align $ vcat [ "tbs <- cauterizePack t"
                                                        , "let tagbs = B.pack tag" <> ain
                                                        , "lbs <- lenAsBs ((B.length tbs) + (B.length tagbs))"
                                                        , "return $ lbs `B.append` tagbs `B.append` tbs"
                                                        ])
                                       ]
                    in t <$> is 

    utils = vcat [ "lenAsBs :: Integral a => a -> Either String B.ByteString"
                 , "lenAsBs l = let l' = fromIntegral l ::" <+> byteCountToWordDoc (AI.aiDataLength ai)
                           <+> "in cauterizePack l'"
                 ]

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
mkAltSize t = msgName t <+> "ty -> cautSize ty"

mkAltMinSize :: AI.AiType -> Doc
mkAltMinSize t = msgName t <+> "ty -> minSize ty"

mkAltMaxSize :: AI.AiType -> Doc
mkAltMaxSize t = msgName t <+> "ty -> maxSize ty"

byteCountToWordDoc :: Integer -> Doc
byteCountToWordDoc c | c == 1 = "U8"
                     | c == 2 = "U16"
                     | c == 4 = "U32"
                     | c == 8 = "U64"
                     | otherwise = error $ "Unable to represent " ++ show c ++ " as a unsigned word."
