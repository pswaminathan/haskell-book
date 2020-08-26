module Words where

import           System.Random (randomRIO)

newtype WordList = WordList [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength :: String -> Bool
          gameLength wd = let l = length wd
                           in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIdx <- randomRIO (0, length wl - 1)
  return $ wl !! randomIdx

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
