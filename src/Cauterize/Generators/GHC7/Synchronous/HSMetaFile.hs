{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generators.GHC7.Synchronous.HSMetaFile
  ( hsMetaFileName
  , renderMetaFile
  ) where

import Cauterize.Specification
import qualified Cauterize.Meta as M

import Cauterize.Generators.GHC7.Synchronous.Common

import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

renderMetaFile :: Spec -> M.Meta -> T.Text
renderMetaFile spec meta = displayT . r $ hsMod <> linebreak <$> parts
  where
    -- tm = specTypeMap spec
    n = specName spec
    r = renderPretty 0.4 80
    ts = M.metaTypes meta
    metaTypeName = (text . nameToCapHsName) n <> "Meta"
    parts = vcat [ imports
                 , linebreak
                 , typeDecl
                 , linebreak
                 , metaHeader
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

    hsMod = "module Cauterize." <> (text . nameToCapHsName $ n) <> "Meta" <+> "where"

    imports = vcat [ "import Cauterize." <> (text . nameToCapHsName $ n)
                   , "import Cauterize.Generators.GHC7.SynchronousSupport"
                   , "import qualified Data.ByteString as B"
                   , "import Data.Word"
                   , "import Control.Monad"
                   , "import Test.QuickCheck.Arbitrary"
                   , "import Test.QuickCheck.Gen"
                   ]

    metaHeader = let htn = (text . nameToCapHsName $ n) <> "MetaHeader"
                 in "data" <+> htn <+> "=" <+> htn <+> byteCountToWordDoc (M.metaDataLength meta) <+> "[Word8]" <+> "deriving (Show, Ord, Eq)"

    typeDecl = "data " <> metaTypeName <> typeAlts
                       <> linebreak <> indent 2 "deriving (Show, Eq, Ord)"
    typeAlts = encloseSep " = " empty " | " $ map mkAlt ts

    typeTagLength = vcat [ "typeTagLength :: Num a => a"
                         , "typeTagLength =" <+> integer (M.metaTypeLength meta)
                         ]
    typeTags = vcat $ map mkTag ts

    dataTagLength = vcat [ "dataTagLength :: Num a => a"
                         , "dataTagLength =" <+> integer (M.metaDataLength meta)
                         ]

    typeSize = let i = "instance CauterizeSize" <+> metaTypeName <+> "where"
                   overhead = integer $ M.metaTypeLength meta + M.metaDataLength meta
                   cs = indent 2 $ vcat $ "cautSize t = fmap (" <> overhead <+> "+) $ case t of" : map (indent 29 . mkAltSize) ts
                   mis = indent 2 $ vcat $ "minSize t =" <+> overhead <+> "+ case t of" : map (indent 19 . mkAltMinSize) ts
                   mas = indent 2 $ vcat $ "maxSize t =" <+> overhead <+> "+ case t of" : map (indent 19 . mkAltMaxSize) ts
               in i <$> cs <$> mis <$> mas

    typeSerialize = let d = "aiPack" <> metaTypeName
                        t = d <+> "::" <+> metaTypeName <+> "-> Either String B.ByteString"
                        mk (M.MetaType metan _) =
                          let metan' = (text . nameToCapHsName) metan
                          in d <+> parens ("Msg" <> metan' <+> "t") <+> "=" <+> mkBody metan'
                        is = vcat $ map mk ts
                        mkBody metan =
                          align $ vcat ["do" <+> align ( vcat [ "tbs <- cauterizePack t"
                                                              , "let tagbs = B.pack tag" <> metan
                                                              , "lbs <- lenAsBs (B.length tbs)"
                                                              , "return $ lbs `B.append` tagbs `B.append` tbs" ])]
                    in t <$> is
    hdrName = (text . nameToCapHsName . M.metaName) meta <> "MetaHeader"
    typeDeserializeHeader = let d = "aiUnpack" <> metaTypeName <> "Header b = do" <+> rest
                                t = "aiUnpack" <> metaTypeName <> "Header" <+> "::" <+> "B.ByteString -> Either String (B.ByteString," <+> hdrName <> ")"
                            in vcat [t, d]
      where
        rest = align $ vcat [ "let (len,rest) = B.splitAt dataTagLength b"
                            , "let (tag,rest') = B.splitAt typeTagLength rest"
                            , "l <- cauterizeUnpack len"
                            , "return (rest', " <+> hdrName <+> "l (B.unpack tag))"
                            ]
    typeDeserializeData = let d = "aiUnpack" <> metaTypeName <> "Data (" <> hdrName <+> " len tag) b = do" <+> rest
                              t = "aiUnpack" <> metaTypeName <> "Data" <+> "::" <+> hdrName <+> "-> B.ByteString -> Either String (B.ByteString," <+> metaTypeName <> ")"
                          in vcat [t, d, theWhere]
      where
        indent' = indent 10
        rest = align $ vcat $ [ "d <- case tag of"
                              ] ++ matches ++ [defMatch] ++
                              [ "return (rest, d)"
                              ]
        matches = map mkMatch ts
        mkMatch (M.MetaType tn p) = indent' "[" <> hcat (punctuate ", " (map (text . T.pack . show) p)) <> "] -> liftM Msg" <> text (nameToCapHsName tn) <+> "(cauterizeUnpack dataBS)"
        defMatch = indent' "u -> Left $ \"Unexpected tag: \" ++ show u"
        theWhere = indent 2 "where" <$> indent 4 "(dataBS, rest) = B.splitAt (fromIntegral len) b"

    instanceArbitrary =
      let inst = "instance Arbitrary" <+> metaTypeName <+> "where"
          decl = indent 2 (vcat [ "arbitrary = oneof [" <+> hcat (punctuate "," (map (("arb" <>) . metaHsName) ts)) <+> "]"
                                , "  where"
                                ]) <$> rest
          rest = indent 6 $ align $ vcat $ map arbMetaType ts
          metaHsName (M.MetaType tname _) = text . nameToCapHsName $ tname
          arbMetaType t = let n' = metaHsName t
                      in "arb" <> n' <+> "= liftM Msg" <> n' <+> "arbitrary"
      in inst <$> decl

    utils = vcat [ "lenAsBs :: Integral a => a -> Either String B.ByteString"
                 , "lenAsBs l = let l' = fromIntegral l ::" <+> byteCountToWordDoc (M.metaDataLength meta)
                           <+> "in cauterizePack l'"
                 ]

mkTag :: M.MetaType -> Doc
mkTag (M.MetaType n p) =
  let n' = "tag" <> (text . nameToCapHsName) n
  in vcat [ n' <+> ":: [U8]"
          , n' <+> "= [" <+> hcat (punctuate  "," (map (text . T.pack . show) p)) <+> "]"
          ]

hsTypeName :: M.MetaType -> Doc
hsTypeName = text . nameToCapHsName . M.metaTypeName

msgName :: M.MetaType -> Doc
msgName t = "Msg" <> hsTypeName t

mkAlt :: M.MetaType -> Doc
mkAlt t = msgName t <+> (text . nameToCapHsName . M.metaTypeName) t

mkAltSize :: M.MetaType -> Doc
mkAltSize t = msgName t <+> "ty -> cautSize ty"

mkAltMinSize :: M.MetaType -> Doc
mkAltMinSize t = msgName t <+> "ty -> minSize ty"

mkAltMaxSize :: M.MetaType -> Doc
mkAltMaxSize t = msgName t <+> "ty -> maxSize ty"

byteCountToWordDoc :: Integer -> Doc
byteCountToWordDoc c | c == 1 = "U8"
                     | c == 2 = "U16"
                     | c == 4 = "U32"
                     | c == 8 = "U64"
                     | otherwise = error $ "Unable to represent " ++ show c ++ " as a unsigned word."
