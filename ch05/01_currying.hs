module Currying where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

nonsense :: Bool -> Integer
nonsense True  = 805
nonsense False = 31337

curriedFn :: Integer
          -> Bool
          -> Integer
curriedFn i b = i + (nonsense b)

uncurriedFn :: (Integer, Bool)
            -> Integer
uncurriedFn (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer
           -> Bool
           -> Integer
anonNested = \i -> \b -> i + (nonsense b)
