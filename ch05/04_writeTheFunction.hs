module Ch5WriteTheFunction where


myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ (xToY x))

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
-- r a = a
-- r a = reverse a
r a = take 5 a

co :: (b -> c)
   -> (a -> b)
   -> a
   -> c
co bToC aToB a =
  bToC (aToB a)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
