module Ch7PatternMatching where

funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "wut"


pal xs =
  case xs == reverse xs of
    True  -> "yes"
    False -> "no"

pal' xs =
  case y of
    True  -> "yes"
    False -> "no"
  where y = xs == reverse xs


functionC x y = if (x > y)
                   then x
                else y

functionC' x y = case x > y of
                   True  -> x
                   False -> y

withOurPowersCombined x y =
  case compare x y of
    LT -> y
    GT -> x
    EQ -> x + y


ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n = case even n of
                  True  -> n + 2
                  False -> n


nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


