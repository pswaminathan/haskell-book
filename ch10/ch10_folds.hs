module Ch10Folds where


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)


myFoldr' :: (a -> b -> b) -> b -> [a] -> b
myFoldr' f z xs =
  case xs of
    []     -> z
    (x:xs) -> f x (myFoldr' f z xs)


-- myFoldl :: (b -> a -> b) -> b -> [a] -> b
-- myFoldl f acc [] = acc
