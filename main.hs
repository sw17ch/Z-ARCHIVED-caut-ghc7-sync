module Main where

import Cauterize.Specification
import qualified Cauterize.AI as AI
import Cauterize.Generators.Hs2010.Synchronous.HSFile
import Cauterize.Generators.Hs2010.Synchronous.HSAiFile
import Cauterize.Generators.Hs2010.Synchronous.TestServer
import Cauterize.Generators.Hs2010.Synchronous.Common

import Options.Applicative
import System.Directory
import System.FilePath.Posix

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Paths_caut_hs2010_synchronous

data CautHs2010Opts = CautHs2010Opts
  { inputFile :: String
  , aiInputFile :: Maybe String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser CautHs2010Opts
optParser = CautHs2010Opts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> (\m -> nullOption $ reader (\v -> return $ Just v) `mappend` m)
    ( long "ai-input"
   <> metavar "AI_FILE_PATH"
   <> value Nothing
   <> help "Input Agnostic Interface specification file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo CautHs2010Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

runWithOptions :: (CautHs2010Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions cautHs2010

cautHs2010 :: CautHs2010Opts -> IO ()
cautHs2010 opts = do
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
        Right s' -> case aiInputFile opts of
                      Nothing -> render s' Nothing out
                      Just aif -> do
                        aif' <- AI.parseFile aif
                        case aif' of
                          Left e -> print e
                          Right ai -> render s' (Just ai) out

createFullPath :: FilePath -> [FilePath] -> IO FilePath
createFullPath p [] = return p
createFullPath p (d:ds) = let n = p `combine` d
                          in createDirectory n >> createFullPath n ds

render :: Spec -> Maybe AI.Ai -> FilePath -> IO ()
render spec mai path = do
  createDirectory path
  createDirectory root

  setup_hs <- getDataFileName "Setup.hs"
  license <- getDataFileName "lib_LICENSE"

  cabalFileData <- cabalFile

  T.writeFile (root `combine` hsFileName spec) hsFile

  case mai of
    Just ai -> do
      T.writeFile (root `combine` hsAiFileName spec) (hsAiFile ai)
      T.writeFile (path `combine` hsTsFileName spec) (renderTsFile ai)
    Nothing -> return ()

  Prelude.writeFile (path `combine` cabalFileName) cabalFileData
  copyFile setup_hs (path `combine` "Setup.hs")
  copyFile license (path `combine` "LICENSE")

  where
    root = path `combine` "Cauterize"
    hsFile = renderHSFile spec
    hsAiFile = renderAIFile spec
    cabalFileName = "caut-generated-" ++ libName spec ++ ".cabal"
    cabalFile = do
      setup_hs <- getDataFileName "lib.cabal"
      c <- Prelude.readFile setup_hs
      let name = "caut-generated-" ++ libName spec
      let nameLine = "name:                " ++ name  ++ "\n"
      let n = T.unpack $ nameToCapHsName $ T.pack (libName spec)
      let exposed = "  exposed-modules:     Cauterize." ++ n ++ ", Cauterize." ++ n ++ "AI\n"
      let exec = unlines [ "executable test_server"
                         , "  ghc-options: -Wall"
                         , "  ghc-options: -Wall"
                         , "  default-language: Haskell2010"
                         , "  main-is: test_server.hs"
                         , "  build-depends: base >=4.6 && <4.8,"
                         , "                 bytes,"
                         , "                 bytestring,"
                         , "                 QuickCheck >= 2.7.6,"
                         , "                 vector >= 0.10.9.1,"
                         , "                 mtl >= 2.2.1,"
                         , "                 transformers >= 0.4.1.0,"
                         , "                 cereal >= 0.4.0.1,"
                         , "                 caut-hs2010-synchronous-support >= 0.1,"
                         , "                 network,"
                         , "                 " ++ name ++ ""
                         ]

      return $ nameLine ++ c ++ exposed ++ "\n" ++ exec
