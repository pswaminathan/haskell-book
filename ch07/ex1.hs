module Ch7Ex1 where

-- addOne x = x + 1
addOne = \x -> x + 1

addOneIfOdd n = case odd n of
                  True  -> f n
                  False -> n
                  where f = \n -> n + 1

addFive x y = (if x > y
                  then y
              else x) + 5

addFiveTwo = \x y -> (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x
