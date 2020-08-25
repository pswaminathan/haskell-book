{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

{-
x = 5
y = x + 5
w = y * 10
z y = y * 10
f = 4 / 5
-}

x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z

bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10

a = 12 + b
b = 10000 * c
c = 10

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ (xToY x))
