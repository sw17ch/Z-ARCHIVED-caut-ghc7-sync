module Main where

import Cauterize.Specification
import Cauterize.Generators.Hs2010.Synchronous.HSFile

import Options.Applicative
import System.Directory
import System.FilePath.Posix

import Data.Text.Lazy.IO as T

data CautHs2010Opts = CautHs2010Opts
  { inputFile :: String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser CautHs2010Opts
optParser = CautHs2010Opts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
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

render :: Spec -> FilePath -> IO ()
render spec path = do
  createDirectory path
  createDirectory root
  T.writeFile (root `combine` hsFileName spec) hsFile
  where
    root = path `combine` "Cauterize"
    hsFile = renderHSFile spec
