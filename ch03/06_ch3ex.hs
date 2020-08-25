module Ch3Ex where

exclaim :: String -> String
exclaim x = x ++ "!"

char5 :: String -> String
char5 x = take 1 (drop 4 x)

drop9 :: String -> String
drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2
-- thirdLetter x = head $ drop 2 x
-- thirdLetter x = head (drop 2 x)

letterIndex :: Int -> Char
letterIndex i = str !! i
  where str = "Curry is awesome!"

rvrs :: String
rvrs = third ++ " " ++ second ++ " " ++ first
  where input = "Curry is awesome!"
        first = take 5 input
        second = take 2 (drop 6 input)
        third = take 7 (drop 9 input)
