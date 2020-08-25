module Ch10Exercises where


-- warm-up: no folds

stops = "pbtdkg"
vowels = "aeiou"

combos :: [(Char, Char, Char)]
combos = [(s, v, st)
    | s <- stops
         , v <- vowels
         , st <- stops
         , s == 'p'
         ]


nouns = ["Jim", "Bob", "house"]
verbs = ["gave", "built"]

phrases = [(noun, verb, noun')
          | noun <- nouns
          , verb <- verbs
          , noun' <- nouns]


avgWordLength :: Fractional a => String -> a
avgWordLength x =
    (/) (fromIntegral $ sum $ map length ws)
        (fromIntegral $ length ws)
    where ws = words x




--
-- working: rewriting using folds
--

myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f = foldr (\a b -> f a || b) False
myAny f = foldr ((||) . f) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 needle = any (\x -> x == needle)

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x ys -> if f x then x:ys else ys) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy cmp (x:xs) = foldl (\x y -> if cmp x y == GT then x else y) x xs
-- n.b. why foldl here? Well, the specification is that we are looking for the
-- *last* value for which cmp returns GT. One of the main benefits of foldr is
-- that it is non-strict in the right argument. But due to the definition,
-- we want to traverse the whole list anyway. Returning the last value implies
-- we want the parentheses to start from the left: (((x cmp y) cmp z) cmp i),
-- i.e. that we want to be left-associative when determining the answer.
-- This also allows us to use the head of the list as the accumulator.
-- This explanation also translates to below


myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy cmp (x:xs) = foldl (\x y -> if cmp x y == LT then x else y) x xs
