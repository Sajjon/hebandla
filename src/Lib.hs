module Lib
    ( parseCorpus,
      Config (..),
      Input (..),
      Output (..)
    ) where

import System.Environment
import Data.List
import System.FilePath
import System.Directory
import Control.Applicative

data Input = FileInput FilePath | StdInput deriving (Eq, Show)
data Output = FileOutput FilePath | StdOutput deriving (Eq, Show)

--fileInput :: Parser Input
--fileInput = FileInput <$> strOption
--  (  long "file"
--  <> short 'f'
--  <> metavar "FILENAME"
--  <> help "Input file" )
--
--stdInput :: Parser Input
--stdInput = flag' StdInput
--  (  long "stdin"
--  <> help "Read from stdin" )

data Config = Config
    { input :: Input
    , minimumWordLength :: Int
    , maximumWordLength :: Int
    , output :: Output
    } deriving (Eq, Show)

newtype Bip39Word = Bip39Word String deriving (Eq, Ord, Show)
newtype Bip39WordList = Bip39WordList [Bip39Word] deriving (Eq, Ord, Show)

--validateInputFile :: Input -> IO Bool
--validateInputFile (FileInput inputFile) = return doesFileExist inputFile
--validateInputFile StdInput = return True

validateConfig :: Config -> IO ()
validateConfig (Config _ minWL maxWL _)
    | minWL >= maxWL                    = error "'Min' word length must be less than 'max'"
    | minWL <= 2                        = error "Word length must be greater than 2 chars"
    | maxWL >= 20                       = error "Word length should never exceed 20 chars."
    | otherwise                         = return ()

parseCorpus :: Config -> IO () -- Bip39WordList
parseCorpus config = do
    _ <- validateConfig config
    putStrLn $ "Hello, you entered the following config: " ++ (show config)

