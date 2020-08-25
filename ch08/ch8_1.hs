module Ch8_1 where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

fact :: Integer -> Integer
fact 0 = 1
fact n = n * factorial (n - 1)

factorial :: Integer -> Integer
factorial n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n * factorial (n - 1)


applyTimes :: (Eq a, Num a)
           => a
           -> (b -> b)
           -> b
           -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n



{-

applyTimes 5 (+1) 5
= (+1) . applyTimes 4 (+1) $ 5
= (+1) . (+1) . applyTimes 3 (+1) $ 5
= (+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
= (+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5
= (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
= (+1) . (+1) . (+1) . (+1) . (+1) . 5
= 10

-}


-- f :: Bool -> Int
-- f True = error "blah"
-- f False = 0

f :: Bool -> Maybe Int
f True = Just 1
f _    = Nothing

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


intDiv :: Integral a => a -> a -> a
intDiv divisor dividend
    | divisor < dividend = 0
    | otherwise = 1 + (intDiv (divisor - dividend) dividend)

divBy :: Integral a => a -> a -> (a, a)
divBy num denom =
  go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count +1)


data DivisionResult = Result Integer
    | DividedByZero
    deriving (Show, Eq)

dividedBy :: Integer -> Integer -> (DivisionResult, Integer)
dividedBy _ 0 = (DividedByZero, 0)
dividedBy num denom =
  go (abs num) (abs denom) 0 (signum num * signum denom)
    where go n d count sign
            | n < d = (Result $ sign * count, n)
            | otherwise = go (n - d) d (count + 1) sign



cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

{-
go (15 - 2) 2 1
go (13 - 2) 2 2 .. 11, 3 .. 9, 4 .. 7, 5 .. 5, 6 .. 3, 7 ..
go 1 < 2 = (7, 1)
-}

-- sumUpTo :: (Ord a, Num a) => a -> a
-- sumUpTo n
--     | n < 0 = 0
--     | n == 0 = 0
--     | otherwise = n + sumUpTo (n - 1)

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)


multBy :: (Integral a) => a -> a -> a
multBy a 1 = a
multBy a b = a + multBy a (b - 1)





mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
