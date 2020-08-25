module LanguageExercises where

import           Data.Char (toUpper)
import           Data.List (isSuffixOf)

capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (s:ss) = toUpper s : ss

capitalizeParagraph :: String -> String
capitalizeParagraph = go . words . capitalizeWord
    where go :: [String] -> String
          go [] = ""
          go (x:[]) = x
          go (x : rest@(y:xs)) = if "." `isSuffixOf` x
                                    then x ++ " " ++ go (capitalizeWord y : xs)
                                    else x ++ " " ++  go rest

