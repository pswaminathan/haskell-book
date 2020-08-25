module Ch8_WordNumber where

import           Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n
    | not $ elem n [0..9] = error "must be single digit"
    | otherwise = case n of
                  0 -> "zero"
                  1 -> "one"
                  2 -> "two"
                  3 -> "three"
                  4 -> "four"
                  5 -> "five"
                  6 -> "six"
                  7 -> "seven"
                  8 -> "eight"
                  9 -> "nine"

digits :: Int -> [Int]
digits n =
  go (abs n) []
    where go n acc
            | n < 10    = (n:acc)
            | otherwise = go (div n 10) ((mod n 10):acc)

wordNumber :: Int -> String
wordNumber n = intercalate "-" words
  where words = map digitToWord $ digits n
