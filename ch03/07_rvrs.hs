module Reverse where

rvrs :: String -> String
rvrs input = third ++ " " ++ second ++ " " ++ first
  where first = take 5 input
        second = take 2 (drop 6 input)
        third = take 7 (drop 9 input)

main :: IO ()
main = print $ rvrs "Curry is awesome!"
