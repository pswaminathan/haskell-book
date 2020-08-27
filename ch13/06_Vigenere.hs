module Vigenere where

import           Control.Monad (forever)
import           Data.Char

-- keyword = "ALLY"

-- input = "MEET AT DAWN"

main :: IO ()
main =
  let vig = mkVigenere "ALLY"
   in forever $ do
     putStrLn ""
     putStr "Enter a word or phrase: "
     word <- getLine
     putStrLn $ "Your phrase encodes to: " ++ vig word



(.>) = flip (.)

capAndStrip :: String -> String
capAndStrip = map toUpper . filter isAlpha

toKeyword :: String -> String -> [(Char, Char)]
toKeyword keyword = zip (cycle keyword) . capAndStrip

toShiftNum :: Char -> Int
toShiftNum c = ord c - 65

bound :: Int -> Int
bound num = 65 + mod (num - 65) 26

shift :: Char -> Char -> Char
shift key char = chr $ bound (ord char + toShiftNum key)

unshift :: Char -> Char -> Char
unshift key char = chr $ bound (ord char - toShiftNum key)

encode :: String -> String -> String
encode keyword = map (uncurry shift) . (toKeyword keyword)
-- I'm referring to the (.>) operator defined in line 9 as the pipeline
-- operator. I prefer this to the function composition operator, because
-- we think left-to-right. I see this and it naturally fits with the
-- mental model of "first, toKeyword, then shift each character."
-- Thus, the above is equivalent to:
-- encode = toKeyword .> map (uncurry shift)

decode :: String -> String -> String
decode keyword = map (uncurry unshift) . (toKeyword keyword)


mkVigenere :: String -> (String -> String)
mkVigenere keyword = encode keyword

mkUnVigenere :: String -> String -> String
mkUnVigenere keyword = decode keyword
