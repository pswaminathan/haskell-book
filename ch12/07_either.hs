module MyEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr chooseLeft []
    where chooseLeft (Left x) acc  = x : acc
          chooseLeft (Right y) acc = acc


rights' :: [Either a b] -> [b]
rights' = foldr chooseRight []
    where chooseRight (Left x) acc  = acc
          chooseRight (Right y) acc = y : acc


partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' = go ([], [])
    where go ret []                  = ret
          go (ls, rs) (Left x : xs)  = go (ls ++ [x], rs) xs
          go (ls, rs) (Right x : xs) = go (ls, rs ++ [x]) xs

-- This one is really simple, but necessarily iterates through the list
-- twice. However, what it trades for in time complexity it might make up
-- for in space complexity (and the general mental overhead of the above).
-- Further, the above implementation appends to the end of the list, which
-- is inefficient.
partitionEithers'' xs = (lefts' xs, rights' xs)
-- TODO look at implementation

partitionEithers''' :: [Either a b] -> ([a], [b])
partitionEithers''' = foldr (either' left right) ([], [])
    where left a (l, r) = (a:l, r)
          right a (l, r) = (l, a:r)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left x)   = Nothing
eitherMaybe' fn (Right y) = Just (fn y)


either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y


eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' bToC = either' nothing (Just . bToC)
    where nothing _ = Nothing
