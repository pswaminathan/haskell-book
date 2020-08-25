module Ch9StdFns where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True
              else myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x then True
                 else myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem i (x:xs) = if i == x then True
                  else myElem i xs

myElem' i xs = any (== i) xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs =
  go xs []
    where go [] acc     = acc
          go (x:xs) acc = go xs (x:acc)


squish :: [[a]] -> [a]
squish [] = []
squish xxs =
  go xxs []
    where go [] acc     = acc
          go (x:xs) acc = go xs (acc ++ x)

squishNT :: [[a]] -> [a]
squishNT []       = []
squishNT (xs:xxs) = xs ++ squishNT xxs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs =
  go f xs []
    where go f [] acc     = acc
          go f (x:xs) acc = go f xs (acc ++ (f x))

squishMapNT :: (a -> [b]) -> [a] -> [b]
squishMapNT _ []     = []
squishMapNT f (x:xs) = (f x) ++ squishMapNT f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x:xs) =
  go cmp xs x
    where go cmp [] cur = cur
          go cmp (y:ys) cur = case (cmp y cur) of
                                LT -> go cmp ys cur
                                EQ -> go cmp ys y
                                GT -> go cmp ys y


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x:xs) =
  go cmp xs x
    where go cmp [] cur = cur
          go cmp (y:ys) cur = case (cmp y cur) of
                                LT -> go cmp ys y
                                EQ -> go cmp ys y
                                GT -> go cmp ys cur


myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
