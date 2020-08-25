module Ch7Ex3 where

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = x `div` 10
        d = xLast `mod` 10


tensDigit :: Integral a => a -> a
tensDigit x =
  let tup = divMod x 10
   in flip mod 10 . fst $ tup


hunsD :: Integral a => a -> a
hunsD x = d2
  where d = fst . divMod x $ 100
        d2 = mod d 10


foldBool :: a -> a -> Bool -> a
foldBool x y b
    | b = y
    | otherwise = x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = case b of
                    True  -> x
                    False -> y


foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y


g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)




