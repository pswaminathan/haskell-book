module SplitOn where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep lst =
    go sep lst []
      where go sep lst acc
                | not $ elem sep lst = reverse $ lst : acc
                | otherwise          = go sep t (h : acc)
                    where h = takeWhile (/= sep) lst
                          t = tail $ dropWhile (/= sep) lst
