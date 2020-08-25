module MyWords where

myWords :: String -> [String]
-- myWords words = h : t
--     where h = takeWhile (/= ' ') words
--           t = myWords . tail $ dropWhile (/= ' ') words
myWords words
    | not $ elem ' ' words = [words]
    | otherwise = let h = takeWhile (/= ' ') words
                      t = myWords $ tail $ dropWhile (/= ' ') words
                  in h : t
