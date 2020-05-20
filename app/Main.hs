module Main where

import Lib
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.FilePath

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "inputFile"
  <> short 'i'
  <> showDefault
  <> value "corpus_first_1000_lines.txt"
  <> metavar "FILENAME"
  <> help "The input file containing language statistics (corpus), one word per line." )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read language statistics (corpus) from stdin, instead from a file." )
  
parseInput :: Parser Input
parseInput = fileInput <|> stdInput

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  long "outputFile"
  <> short 'o'
  <> showDefault
  <> value "bip39list.txt"
  <> metavar "FILENAME"
  <> help "Output file for BIP39 word list, one word per line." )

stdOutput :: Parser Output
stdOutput = flag' StdOutput
  (  long "stdout"
  <> help "Output the final BIP39 word list to stdout, instead of a file." )
  
parseOutput :: Parser Output
parseOutput = fileOutput <|> stdOutput

parseMinWordLength :: Parser Int
parseMinWordLength = option auto
    (long "minWordLength" <>
     short 'm' <>
     help "Minimum allowed length of a word in the final BIP39 word list." <>
     showDefault <>
     value 4 <> -- the default value according to BIP39 standards
     metavar "INT")
  
parseMaxWordLength :: Parser Int
parseMaxWordLength = option auto
    (long "maxWordLength" <>
     short 'M' <>
     help "Maximum allowed length of a word in the final BIP39 word list." <>
     showDefault <>
     value 11 <> -- BIP39 standard is a max word length of 8 chars, but Swedish words are quite long
     metavar "INT")

argsParser :: Parser Config
argsParser =
    Config <$> -- CLI options parser definition
    parseInput <*>
    parseMinWordLength <*>
    parseMaxWordLength <*>
    parseOutput

main :: IO ()
main = parseCorpus =<< execParser parseCLIArguments
  where
    parseCLIArguments = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "A program for constructing a BIP39 wordlist from some corpus for some language. The corpus need to contain word + metadata, one per line, with the most frequently used word of the language at line 0."
     <> header "hebandla - a BIP39 wordlist construction program." )