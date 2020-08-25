{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ex5Try2 where

class TooMany a where
  tooMany :: a -> Bool


instance TooMany Int where
  tooMany n = n > 9000


newtype Goats = Goats Int
  deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 42

newtype Cows = Cows Int
  deriving (Eq, Show, TooMany)


--
-- exercises
--

newtype IntStringInline = IntString (Int, String)
  deriving (Eq, Show)

instance TooMany IntStringInline where
  tooMany (IntString (n, _)) = n > 1000


type IntStringAlias = (Int, String)

instance TooMany IntStringAlias where
  tooMany (n, _) = n > 3000


newtype IntIntInline = IntInt (Int, Int)
  deriving (Eq, Show)

instance TooMany IntIntInline where
  tooMany (IntInt (x, y)) = x + y > 9000


instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y
