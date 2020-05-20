{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( parseCorpus,
      Config (..),
      Input (..),
      Output (..)
    ) where

import System.Environment
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Char
import System.FilePath
import System.Directory
import Control.Applicative

data Input = FileInput FilePath | StdInput deriving (Eq, Show)
data Output = FileOutput FilePath | StdOutput deriving (Eq, Show)

data Config = Config
    { input :: Input
    , minimumWordLength :: Int
    , maximumWordLength :: Int
    , output :: Output
    } deriving (Eq, Show)

newtype Bip39Word = Bip39Word ParsedLine deriving (Eq, Ord, Show)
newtype Bip39WordList = Bip39WordList [Bip39Word] deriving (Eq, Ord, Show)

bip39WordFromList :: Bip39WordList -> [Bip39Word]
bip39WordFromList (Bip39WordList list) = list

validateInput :: Input -> IO Bool
validateInput StdInput = return True
validateInput (FileInput inputFile) = doesFileExist inputFile

validateConfig :: Config -> IO ()
validateConfig (Config input minWL maxWL _)
    | minWL >= maxWL    = error "'Min' word length must be less than 'max'"
    | minWL <= 2        = error "Word length must be greater than 2 chars"
    | maxWL >= 20       = error "Word length should never exceed 20 chars."
    | otherwise = do
        isValidInput <- validateInput input
        if isValidInput
            then return ()
            else error "Input file does not exist"

outputBip39Word :: Output -> Bip39Word -> IO ()
outputBip39Word (FileOutput outputFile) bipWord = writeFile outputFile $ show bipWord
outputBip39Word StdOutput bipWord = print bipWord

outputBip39List :: Bip39WordList -> Output -> IO ()
outputBip39List bipList output = do
    let outputWord = outputBip39Word output
    mapM_ outputWord $ bip39WordFromList bipList

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile = fmap lines . readFile

readNLinesFromStdin :: Int -> IO [String]
readNLinesFromStdin n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- readNLinesFromStdin (n-1)
        return (x:xs)


readLines :: Input -> IO [String]
readLines StdInput = readNLinesFromStdin 10000
readLines (FileInput inputFile) = readLinesFromFile inputFile

data PartOfSpeechTag = Adverb | Adjective | Noun | Verb | UNHANDLED String deriving (Eq, Show)

-- String is on unparsed format: `PN.UTR.SIN.DEF.SUB`
-- We only deal with the first part of the tag, i.e. `PN` from the line above
posTagFromString :: String -> PartOfSpeechTag
posTagFromString str
    | pos == "AB" = Adverb
    | pos == "JJ" = Adjective
    | pos == "NN" = Noun
    | pos == "VB" = Verb
    | otherwise = UNHANDLED pos
  where
    pos =
        if '.' `elem` str
            then head $ splitOn "." str
            else str

newtype WordForm = WordForm String deriving (Eq, Ord, Show)
newtype BaseForm = BaseForm String deriving (Eq, Ord, Show)

data Lemgram = Lemgram
    { baseForm :: BaseForm
    , partOfSpeechTag :: PartOfSpeechTag
    , index :: Int
    } deriving (Eq, Show)

newtype Lemgrams = Lemgrams [Lemgram] deriving (Eq, Show)

data ParsedLine = ParsedLine {
    -- Read verbatim from corpus
    wordForm :: WordForm
    , partOfSpeechTag :: PartOfSpeechTag
    , lemgrams :: Lemgrams
    , isCompoundWord :: Bool
    , totalNumberOfOccurrences :: Int -- ought to be based for `Ord`
    , relativeNumberOfOccurrences :: Float

    -- Appended by this program
    , positionInCorpus :: Int
} deriving (Eq, Show)

instance Ord ParsedLine where
    p1 `compare` p2 = totalNumberOfOccurrences p1 `compare` totalNumberOfOccurrences p2

componentsSplitByTab :: String -> [String]
componentsSplitByTab = splitOn "\t"

lowerSwedishChars = ['a'..'z'] ++ ['å', 'ä', 'ö']


--isLowerSwedishChar :: Char -> Bool
--isLowerSwedishChar c = c `elem` lowerSwedishChars

parseWordForm :: String -> Maybe WordForm
parseWordForm str =
    let lowerCase = map toLower str
        validWordContent = not . null $ lowerCase \\ lowerSwedishChars
    in if validWordContent
        then Just $ WordForm lowerCase
        else Nothing

parsedLineFromParseComponents :: Maybe WordForm         -- ^ Possible word form
                              -> PartOfSpeechTag        -- ^ POS tag
                              -> Lemgrams               -- ^ Lemgrams
                              -> Bool                   -- ^ If isCompoundWord or not
                              -> Int                    -- ^ totalNumberOfOccurrences
                              -> Float                    -- ^ relativeNumberOfOccurrences
                              -> Int                    -- ^ position of line in corpus
                              -> Maybe ParsedLine       -- ^ ParsedLine
parsedLineFromParseComponents Nothing _ _ _ _ _ _ = Nothing
parsedLineFromParseComponents (Just wf) pos lg cw tc rc idx = Just $ ParsedLine wf pos lg cw tc rc idx

parseIsCompoundWord :: String -> Bool
parseIsCompoundWord [c] = c == '-'
parseIsCompoundWord _ = False

parseLemgram :: String -> Lemgram
parseLemgram str =
    let [bfStr, posAndIndexMerged] = splitOn ".." str
        bf = BaseForm bfStr
        [posStr, indexStr] = splitOn "." posAndIndexMerged
        index = read indexStr
        pos = posTagFromString $ map toUpper posStr
      in Lemgram bf pos index
--    in case pos of
--        Just posTag -> Lemgram bf posTag index
--        Nothing -> error "Failed to parse POS for Lemgram"

parseLemgrams :: String -> Lemgrams
parseLemgrams str =
    let lemgramStrings = filter (not . null) $ splitOn "|" str
    in Lemgrams $ map parseLemgram lemgramStrings

--
parseLineFromComponents :: [String]             -- ^ Components of a read line, split on some delimiter
                        -> Int                  -- ^ Position of this line in the read corpus
                        -> Maybe ParsedLine     -- ^ If all components are valid we return a `ParsedLine`, otherwise `Nothing`
parseLineFromComponents (wfC:posC:lgC:cwC:tcC:rcC:_) idx =
    let wf  = parseWordForm wfC
        pos = posTagFromString posC
        lgs  = parseLemgrams lgC
        cw  = parseIsCompoundWord cwC
        tc  = read tcC :: Int
        rc  = read rcC :: Float
    in parsedLineFromParseComponents wf pos lgs cw tc rc idx

-- `och	KN	|och..kn.1|	-	243000319	18256.303370`
parseLine :: String         -- ^ read line
          -> Int            -- ^ line index
          -> Maybe ParsedLine
parseLine line lineIndex
    | numberOfComponents /= 6 = error "Incorrect number of components"
    | otherwise = parseLineFromComponents components lineIndex
    where
        components = componentsSplitByTab line
        numberOfComponents = length components

dummyMakeBip39List :: [ParsedLine] -> Bip39WordList
dummyMakeBip39List parsedLines = Bip39WordList $ fmap Bip39Word parsedLines

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

maybee :: [Maybe a] -> [a]
maybee (Nothing:xs) = maybee xs
maybee (Just x:xs)  = x : maybee xs
maybee []           = []

--Input: zipWith (\x y -> 2*x + y) [1..4] [5..8]
--Output: [7,10,13,16]

parseLines :: [String] -> [ParsedLine]
parseLines strings =
    let maybes = zipWith parseLine strings [0 ..]
    in  maybee maybes
--parseLines strings = mapMaybe $ zipWith parseLine strings [0..]
--parseLines strings = mapInd (\s i -> filter  ) strings --mapInd parseLine
--    let indexedList = zipWith  l [0..]
--    mapMaybe $ (mapInd parseLine strings)

parseCorpus :: Config -> IO () -- Bip39WordList
parseCorpus config = do
    validateConfig config
    putStrLn $ "Starting program with config: " ++ show config
    corpusLines <- readLines $ input config
    let parsedLines = parseLines corpusLines
        bip39List = dummyMakeBip39List parsedLines
    outputBip39List bip39List $ output config

