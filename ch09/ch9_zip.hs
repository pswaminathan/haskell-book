module Ch9Zip where

mzip :: [a] -> [b] -> [(a, b)]
mzip _ []          = []
mzip [] _          = []
mzip (x:xs) (y:ys) = (x, y) : mzip xs ys

mzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
mzipWith _ _ []          = []
mzipWith _ [] _          = []
mzipWith f (x:xs) (y:ys) = (f x y) : mzipWith f xs ys

mzip2 :: [a] -> [b] -> [(a, b)]
mzip2 = mzipWith (,)
