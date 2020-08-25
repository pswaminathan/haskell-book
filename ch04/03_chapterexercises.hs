module Ch4EndExercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)

isPalRecur :: (Eq a) => [a] -> Bool
isPalRecur []     = True
isPalRecur [x]    = True
isPalRecur (x:xs) = if x == x'
                       then isPalRecur (init xs)
                    else False
                      where x' = last xs

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0 then x
  else (negate x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+1)
g xs = x (length xs)

h :: (a, b) -> a
h (a, b) = a
