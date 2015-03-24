{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cauterize.Specification
import qualified Cauterize.Meta as M
import Cauterize.Generators.GHC7.Synchronous.HSFile
import Cauterize.Generators.GHC7.Synchronous.HSMetaFile
import Cauterize.Generators.GHC7.Synchronous.TestServer
import Cauterize.Generators.GHC7.Synchronous.Common

import Options.Applicative
import System.Directory
import System.FilePath.Posix

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Paths_caut_ghc7_sync

data CautGHC7Opts = CautGHC7Opts
  { inputFile :: FilePath
  , metaInputFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)

optParser :: Parser CautGHC7Opts
optParser = CautGHC7Opts
  <$> strOption
    ( long "spec"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  -- <*> (\m -> nullOption $ reader (\v -> return $ Just v) `mappend` m)
  <*> strOption
    ( long "meta"
   <> metavar "META_FILE_PATH"
   <> help "Meta interface specification file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo CautGHC7Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

runWithOptions :: (CautGHC7Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions cautGHC7

cautGHC7 :: CautGHC7Opts -> IO ()
cautGHC7 opts = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out
  if fe || de
    then error $ out ++ " already exists."
    else go $ inputFile opts
  where
    out = outputDirectory opts
    go inFile = do
      s <- parseFile inFile
      case s of
        Left e -> print e
        Right s' -> do aif' <- M.parseFile $ metaInputFile opts
                       case aif' of
                         Left e -> print e
                         Right ai -> render s' (Just ai) out

createFullPath :: FilePath -> [FilePath] -> IO FilePath
createFullPath p [] = return p
createFullPath p (d:ds) = let n = p `combine` d
                          in createDirectory n >> createFullPath n ds

render :: Spec -> Maybe M.Meta -> FilePath -> IO ()
render spec mai path = do
  createDirectory path -- ./
  createDirectory srcDir -- ./src
  createDirectory cautDir -- ./src/Cauterize
  createDirectory binDir -- ./test_server

  setup_hs <- getDataFileName "Setup.hs"
  license <- getDataFileName "lib_LICENSE"
  test_server <- getDataFileName "Cauterize/TestServer.hs"

  cabalFileData <- cabalFile

  T.writeFile (cautDir `combine` hsFileName spec) hsFile

  case mai of
    Just ai -> do
      T.writeFile (cautDir `combine` hsMetaFileName spec) (hsMetaFile ai)
      T.writeFile (binDir `combine` "Main.hs") (renderTsFile ai)
    Nothing -> return ()

  T.writeFile (path `combine` cabalFileName) cabalFileData
  copyFile setup_hs (path `combine` "Setup.hs")
  copyFile license (path `combine` "LICENSE")
  copyFile test_server (cautDir `combine` "TestServer.hs")

  where
    srcDir = path `combine` "src"
    binDir = path `combine` "test_server"
    cautDir = srcDir `combine` "Cauterize"

    hsFile = renderHSFile spec
    hsMetaFile = renderMetaFile spec
    cabalFileName = "caut-generated-" ++ (T.unpack . libName) spec ++ ".cabal"
    cabalFile = do
      setup_hs <- getDataFileName "lib.cabal"
      c <- T.readFile setup_hs
      let name = "caut-generated-" `T.append` libName spec
      let nameLine = "name:                " `T.append` name `T.append` "\n"
      let n = nameToCapHsName $ libName spec
      let exposed = T.concat["  exposed-modules:     Cauterize.TestServer, Cauterize.", n, ", Cauterize.", n, "Meta\n"]
      let exec = T.unlines [ "executable test_server"
                           , "  hs-source-dirs: test_server/"
                           , "  ghc-options: -Wall"
                           , "  ghc-options: -Wall"
                           , "  default-language: Haskell2010"
                           , "  main-is: Main.hs"
                           , "  build-depends: base >=4.6 && <4.8,"
                           , "                 bytes,"
                           , "                 bytestring,"
                           , "                 QuickCheck >= 2.7.6,"
                           , "                 vector >= 0.10.9.1,"
                           , "                 mtl >= 2.1.3.1,"
                           , "                 transformers >= 0.4.2.0,"
                           , "                 cereal >= 0.4.0.1,"
                           , "                 caut-ghc7-sync >= 0.1,"
                           , "                 " `T.append` name `T.append` ""
                           ]

      return $ nameLine `T.append` c `T.append` exposed `T.append` "\n" `T.append` exec
