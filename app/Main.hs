module Main where

import Lib
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.FilePath

data ConfigFromArgs = ConfigFromArgs
  { showVersion     :: Bool
  , inputFile       :: String
  , minWordLength   :: Int
  , maxWordLength   :: Int
  , outputFile      :: String } deriving (Eq, Show)

argsParser :: Parser ConfigFromArgs
argsParser =
    ConfigFromArgs <$> -- CLO options parser definition
    version <*>
    input <*>
    minWL <*>
    maxWL <*>
    output
  where
    version =
        switch
            (long "version" <>
             short 'v' <>
             help "Whether to be just show version, or actually run program.")
    --
    -- | Parse input file
    input =
        strOption
            (long "inputFile" <>
             short 'i' <>
             help "corpus input file containing language statistics, one word per line." <>
             showDefault <>
             value "corpus_first_1000_lines.txt" <>
             metavar "FILENAME")
    --
    -- | Parse min word length
    minWL =
        option auto
            (long "minWordLength" <>
             short 'm' <>
             help "Minimum allowed length of a word in the final BIP39 word list." <>
             showDefault <>
             value 4 <> -- the default value according to BIP39 standards
             metavar "INT")
    --
    -- | Parse max word length
    maxWL =
        option auto
            (long "maxWordLength" <>
             short 'M' <>
             help "Maximum allowed length of a word in the final BIP39 word list." <>
             showDefault <>
             value 11 <> -- BIP39 standard is a max word length of 8 chars, but Swedish words are quite long
             metavar "INT")
    --
    -- | Parse output file name
    output =
        strOption
            (long "outputFile" <>
             short 'o' <>
             help "Output file for the final BIP39 word list of 2048 words, one word per line." <>
             showDefault <>
             value "bip39list.txt" <>
             metavar "FILENAME"
             )


main :: IO ()
main = parseCorpusWithArgs =<< execParser parseCLIArguments
  where
    parseCLIArguments = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "A program for constructing a BIP39 wordlist from some corpus for some language. The corpus need to contain word + metadata, one per line, with the most frequently used word of the language at line 0."
     <> header "hebandla - a BIP39 wordlist construction program." )

parseCorpusWithArgs :: ConfigFromArgs -> IO ()
parseCorpusWithArgs (ConfigFromArgs True _ _ _ _) = putStrLn "Version: 0.0.1"
parseCorpusWithArgs (ConfigFromArgs _ inputFile minWL maxWL outputFile) = do
    currentDirectory <- getCurrentDirectory
    let inputFilePath = currentDirectory </> inputFile
        outputFilePath = currentDirectory </> outputFile
        config = Config {
            input = FileInput inputFilePath,
            minimumWordLength = minWL,
            maximumWordLength = maxWL,
            output = FileOutput outputFilePath
        }
    parseCorpus config