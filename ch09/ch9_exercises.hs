module Ch9Exercises where

import           Data.Char

getCaps = filter isUpper

capFirst :: [Char] -> [Char]
capFirst ""     = ""
capFirst (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : capitalize xs

getFirstCap :: String -> Char
getFirstCap (x:_) = toUpper x

getFirstCap' = toUpper . head
