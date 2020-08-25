module Ch6Exercises2 where

import           Data.List (sort)

i :: Num a => a
-- i :: a
i = 1

-- f :: Float
-- f :: Num a => a
-- f = 1.0

-- f :: Float
-- f :: Fractional a => a
f :: RealFrac a => a
f = 1.0

-- freud :: a -> a
freud :: Ord a => a -> a -- but why?
freud x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX


jung :: Ord a => [a] -> a
-- jung :: [Int] -> Int
jung xs = head (sort xs)


-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)


chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB x y = let b = aToB x
               in y == b

arith :: Num b =>
         (a -> b)
      -> Integer
      -> a
      -> b
arith aToB x a = fromInteger x + (aToB a)





