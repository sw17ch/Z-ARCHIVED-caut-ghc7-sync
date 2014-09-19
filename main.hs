module Main where

import Cauterize.Specification
import Cauterize.Generators.Hs2010.Synchronous.HSFile
import Cauterize.Generators.Hs2010.Synchronous.HSAiFile

import Options.Applicative
import System.Directory
import System.FilePath.Posix

import Data.Text.Lazy.IO as T

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
        Right s' -> render s' out

createFullPath :: FilePath -> [FilePath] -> IO FilePath
createFullPath p [] = return p
createFullPath p (d:ds) = let n = p `combine` d
                          in createDirectory n >> createFullPath n ds

render :: Spec -> FilePath -> IO ()
render spec path = do
  createDirectory path
  createDirectory root

  setup_hs <- getDataFileName "Setup.hs"
  license <- getDataFileName "lib_LICENSE"

  cabalFileData <- cabalFile

  T.writeFile (root `combine` hsFileName spec) hsFile
  T.writeFile (root `combine` hsAiFileName spec) hsAiFile
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
      let nameLine = "name:                caut-generated-" ++ libName spec ++ "\n"
      return $ nameLine ++ c
