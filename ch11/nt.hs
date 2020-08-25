{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NewType where

newtype Goats = Goats Int
    deriving (Eq, Show, TooMany)

newtype Cows = Cows Int
    deriving (Eq, Show)


tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 42

instance TooMany Int where
  tooMany n = n > 9000

instance TooMany Integer where
  tooMany n = n > 43

newtype IntTup = IntTup (Int, String)

instance TooMany IntTup where
  tooMany (IntTup (n, s)) = n > 42


type TwoFields = (Int, Int)

instance TooMany TwoFields where
  tooMany (x, y) = x + y > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y
