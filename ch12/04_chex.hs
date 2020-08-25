module ChapterExercises where

import           Data.List (intercalate)

{-
  Determine the kinds:
  1. Given id :: a -> a
     a :: *
  2. r :: a -> f a
     a :: *
     f :: * -> *
-}

notThe :: String -> Maybe String
notThe s = if s == "the"
              then Nothing
              else Just s

replaceThe :: String -> String
replaceThe = intercalate " "
           . map (replaceThe' . notThe)
           . words
  where replaceThe' = maybe "a" id


countThesBeforeVowel :: String -> Integer
countThesBeforeVowel = go 0 . words
    where go :: Integer -> [String] -> Integer
          go acc [] = acc
          go acc (x:[]) = acc
          go acc (x:y:ys) = case (notThe x) of
                              Just _ -> go acc (y:ys)
                              Nothing -> if elem (head y) "AEIOUaeiou"
                                            then go (acc + 1) (y:ys)
                                            else go acc (y:ys)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
    where go :: Integer -> [String] -> Integer
          go acc [] = acc
          go acc (x:[]) = acc
          go acc (x:y:ys)
              | x /= "the" = go acc (y:ys)
              | otherwise = if elem (head y) "AEIOUaeiou"
                               then go (acc + 1) (y:ys)
                               else go acc (y:ys)


countVowels :: String -> Integer
countVowels "" = 0
countVowels (x:xs)
    | elem x "AEIOUuaeiou" = 1 + countVowels xs
    | otherwise = countVowels xs


newtype Word' = Word' String
  deriving (Eq, Show)

allVowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word = go 0 0 word
    where go :: Integer -> Integer -> String -> Maybe Word'
          go vowels consonants ""
              | vowels > consonants = Nothing
              | otherwise = Just $ Word' word
          go vowels consonants (x:xs) =
              if elem x allVowels
                 then go (vowels + 1) consonants xs
                 else go vowels (consonants + 1) xs
