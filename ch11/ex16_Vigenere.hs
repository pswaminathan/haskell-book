module Vigenere where

import           Data.Char

keyword = "ALLY"

-- input = "MEET AT DAWN"

(.>) = flip (.)

capAndStrip :: String -> String
capAndStrip = filter isAlpha . map toUpper

toKeyword :: String -> [(Char, Char)]
toKeyword = zip (cycle keyword) . capAndStrip

toShiftNum :: Char -> Int
toShiftNum c = ord c - 65

bound :: Int -> Int
bound num = 65 + mod (num - 65) 26

shift :: Char -> Char -> Char
shift key char = chr $ bound (ord char + toShiftNum key)

unshift :: Char -> Char -> Char
unshift key char = chr $ bound (ord char - toShiftNum key)

encode :: String -> String
encode = map (uncurry shift) . toKeyword
-- I'm referring to the (.>) operator defined in line 9 as the pipeline
-- operator. I prefer this to the function composition operator, because
-- we think left-to-right. I see this and it naturally fits with the
-- mental model of "first, toKeyword, then shift each character."
-- Thus, the above is equivalent to:
-- encode = toKeyword .> map (uncurry shift)

decode :: String -> String
decode = map (uncurry unshift) . toKeyword
