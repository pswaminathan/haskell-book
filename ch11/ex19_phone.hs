module Phone where

import           Data.Char     (isAlpha, isUpper, toLower, toUpper)
import           Data.Function (on)
import           Data.List     (find, findIndex, group, groupBy, intercalate,
                                maximumBy, sort)


{-
   Answers:
   1. Look at phone
   2. map (stringToTaps phone) convo
   3. map (fingerTaps . (stringToTaps phone)) convo
      or, for total taps, fingerTaps $ concatMap (stringToTaps phone) convo
   4. map mostPopularLetter convo
      map costForMostPopularLetter convo
   5. coolestLetter convo
      coolestWord convo
-}




-- Digit represents a phone button's "number":
-- 0-9, *, +
type Digit = Char
-- Presses represents how many times a button was pressed
type Presses = Int

data Button = Button Digit [Char]
    deriving Show

data Phone = Phone [Button]
    deriving Show

validDigits :: [Digit]
validDigits = ['0'..'9'] ++ ['*', '#']

isValidDigit :: Digit -> Bool
isValidDigit = (flip elem) validDigits

validLetters :: String
validLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '.', ',']

isValidLetter :: Char -> Bool
isValidLetter = (flip elem) validLetters

phone :: Phone
phone = Phone
  [ Button '1' ""    , Button '2' "abc", Button '3' "def"
  , Button '4' "ghi" , Button '5' "jkl", Button '6' "mno"
  , Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz"
  , Button '*' ""    , Button '0' " "  , Button '#' ""
  ]


allChars :: Button -> [Char]
allChars (Button digit chars) = chars ++ [digit]


button :: Phone -> Int -> Button
button (Phone buttons) = (!!) buttons

findButtonForDigit :: Phone -> Digit -> Maybe Button
findButtonForDigit (Phone buttons) digit = find isDigit buttons
    where isDigit (Button dig _) = dig == digit

findButtonForChar :: Phone -> Char -> Maybe Button
findButtonForChar (Phone buttons) char = find hasChar buttons
    where hasChar button = elem char (allChars button)


tapsForChar :: Button -> Char -> Maybe Presses
tapsForChar (Button digit chars) char =
     fmap (+1) $ findIndex (\c -> c == char) (allChars (Button digit chars))




reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone char
    | isUpper char = ('*', 1) : reverseTaps phone (toLower char)
    | otherwise =
          let maybeButton = findButtonForChar phone char
           in maybe [] fn maybeButton
                where fn :: Button -> [(Digit, Presses)]
                      fn (Button digit chars) = maybe [] (\ps -> [(digit, ps)]) (tapsForChar (Button digit chars) char)

stringToTaps :: Phone -> String -> [(Digit, Presses)]
stringToTaps phone = concatMap (reverseTaps phone)

convo :: [String]
convo =
    ["Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol OK. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "OK. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr fn 0
    where fn (_, presses) acc = presses + acc


zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap fn xs = zip xs (map fn xs)


mostPopularLetter :: String -> Char
mostPopularLetter = head
                  . fst
                  . maximumBy (compare `on` snd)
                  . stringToGroups


costForMostPopularLetter :: String -> Presses
costForMostPopularLetter = fingerTaps
                         . concatMap (reverseTaps phone)
                         . fst
                         . maximumBy (compare `on` snd)
                         . stringToGroups


stringToGroups :: String -> [(String, Int)]
stringToGroups = zipMap length
               . group
               . sort
               . map toLower
               . filter isAlpha


stringToWordGroups :: String -> [(String, Int)]
stringToWordGroups = map (\(ss, l) -> (head ss, l))
                   . zipMap length
                   . groupBy (\a b -> a == b)
                   . sort
                   . words
                   . map toLower


coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat


coolestWord :: [String] -> String
coolestWord = fst
            . maximumBy (compare `on` snd)
            . stringToWordGroups
            . intercalate " "
