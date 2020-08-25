module TypeInference1 where

-- f :: Num a => a -> a -> a
f x y = x + y + 3

triple x = tripleItYo x
  where tripleItYo :: Integer -> Integer
        tripleItYo y = y * 3
