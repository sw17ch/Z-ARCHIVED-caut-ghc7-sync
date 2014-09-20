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
                 , aiHeader
                 , linebreak
                 , typeTagLength
                 , dataTagLength
                 , linebreak
                 , typeTags
                 , linebreak
                 , typeSize
                 , linebreak
                 , utils
                 , linebreak
                 , typeSerialize
                 , linebreak
                 , typeDeserializeHeader
                 , typeDeserializeData
                 , instanceArbitrary
                 ]

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <> "AI" <+> "where"

    imports = vcat [ "import Cauterize." <> (text . nameToCapHsName $ n)
                   , "import Cauterize.Support.Hs2010"
                   , "import qualified Data.ByteString as B"
                   , "import Data.Word"
                   , "import Control.Monad"
                   , "import Test.QuickCheck.Arbitrary"
                   , "import Test.QuickCheck.Gen"
                   ]

    aiHeader = let htn = (text . nameToCapHsName $ n) <> "AIHeader"
               in "data" <+> htn <+> "=" <+> htn <+> byteCountToWordDoc (AI.aiDataLength ai) <+> "[Word8]" <+> "deriving (Show, Ord, Eq)"

    typeDecl = "data " <> aiTypeName <> typeAlts
                       <> linebreak <> (indent 2 "deriving (Show, Eq, Ord)")
    typeAlts = encloseSep " = " empty " | " $ map mkAlt ts

    typeTagLength = vcat [ "typeTagLength :: Num a => a"
                         , "typeTagLength =" <+> integer (AI.aiTypeLength ai)
                         ]
    typeTags = vcat $ map mkTag ts

    dataTagLength = vcat [ "dataTagLength :: Num a => a"
                         , "dataTagLength =" <+> integer (AI.aiDataLength ai)
                         ]

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
                                                        ])]
                    in t <$> is 
    hdrName = (text . nameToCapHsName . T.pack . AI.aiName) ai <> "AIHeader"
    typeDeserializeHeader = let d = "aiUnpack" <> aiTypeName <> "Header b = do" <+> rest
                                t = "aiUnpack" <> aiTypeName <> "Header" <+> "::" <+> "B.ByteString -> Either String (B.ByteString," <+> hdrName <> ")"
                            in vcat [t, d]
      where
        rest = align $ vcat $ [ "let (len,rest) = B.splitAt dataTagLength b"
                              , "let (tag,rest') = B.splitAt typeTagLength rest"
                              , "l <- cauterizeUnpack len"
                              , "return (rest', " <+> hdrName <+> "l (B.unpack tag))"
                              ]
    typeDeserializeData = let d = "aiUnpack" <> aiTypeName <> "Data (" <> hdrName <+> " len tag) b = do" <+> rest
                              t = "aiUnpack" <> aiTypeName <> "Data" <+> "::" <+> hdrName <+> "-> B.ByteString -> Either String (B.ByteString," <+> aiTypeName <> ")"
                          in vcat [t, d, theWhere]
      where
        indent' = indent 10 
        rest = align $ vcat $ [ "d <- case tag of"
                              ] ++ matches ++ [defMatch] ++
                              [ "return (rest, d)"
                              ]
        matches = map mkMatch ts
        mkMatch (AI.AiType tn p) = indent' "[" <> hcat (map (text . T.pack . show) p) <> "] -> liftM Msg" <> text (nameToCapHsName $ T.pack tn) <+> "(cauterizeUnpack dataBS)"
        defMatch = indent' "u -> Left $ \"Unexpected tag: \" ++ show u"
        theWhere = indent 2 "where" <$> indent 4 "(dataBS, rest) = B.splitAt (fromIntegral len) b"

    instanceArbitrary =
      let inst = "instance Arbitrary" <+> aiTypeName <+> "where"
          decl = indent 2 (vcat [ "arbitrary = oneof [" <+> (hcat $ punctuate "," (map (("arb" <>) . aiHsName) ts) ) <+> "]" 
                                , "  where"
                                ]) <$> rest
          rest = indent 6 $ align $ vcat $ map arbAiType ts
          aiHsName (AI.AiType tname _) = text . nameToCapHsName . T.pack $ tname
          arbAiType t = let n' = aiHsName t
                      in "arb" <> n' <+> "= liftM Msg" <> n' <+> "arbitrary"
      in inst <$> decl

    utils = vcat [ "lenAsBs :: Integral a => a -> Either String B.ByteString"
                 , "lenAsBs l = let l' = fromIntegral l ::" <+> byteCountToWordDoc (AI.aiDataLength ai)
                           <+> "in cauterizePack l'"
                 ]

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
