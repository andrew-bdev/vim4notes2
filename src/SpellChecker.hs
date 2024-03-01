module SpellChecker where

import Spellcheck
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, fromList)
import System.Directory (doesFileExist)
import Data.List (break, sort)
import Data.Maybe
import Data.String
import Data.Char (isAlpha, toLower)

datasetpath :: String
datasetpath = "prob_map.txt"

boundary :: Double
boundary = 0.0000001

-- given a list of words, finds n most likely similar words
-- returns in the form [(original word 1, [most likely words]), ... ]
batchspellcheck :: [String] -> Int -> IO [(String, [String])]
batchspellcheck lst n = do
    probs <- loaddata datasetpath
    let corrections = [(wrd, topncorrections wrd n probs) | wrd <- lst]
    return corrections

-- given a single word, finds n most likely similar words
-- returns in the form [original word, most likely word 1, ... most likely word n]
spellcheck :: String -> Int -> IO [String]
spellcheck word n = do
    probs <- loaddata datasetpath
    let corrections = word : topncorrections word n probs
    return corrections

-- takes an unbroken string of text, returns list of words from text that are most likely to be a spelling mistake (below a probability boundary)
findwrongwords :: String -> IO [String]
findwrongwords text = do
    probs <- loaddata datasetpath
    let wordlst = words text
    let cleanedlst = [[toLower x| x <- wrd] | wrd <- wordlst, (filter (isAlpha) wrd)==wrd]
    let wrongwords = leastlikelywords cleanedlst boundary probs
    return wrongwords

-- does it all. takes list of unbroken text (ex. "ncie to meet yo, my nme is sofia"), 
--      finds least likely words (ex. ["ncie", "yo", "nme"]), 
--      and returns suggested corrections (ex. [("ncie", ["nice", "ice", ...])... ])
findandcorrect :: String -> IO [(String, [String])]
findandcorrect text = do
    wrongwords <- findwrongwords text
    batchspellcheck wrongwords 5

loaddata :: FilePath -> IO (Map String Double)
loaddata filepath = do
    dictString <- readFile filepath
    let dict = read dictString :: (Map String Double)
    return dict
