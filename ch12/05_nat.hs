module Nat where

data Nat = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n = case (compare n 0) of
                   LT -> Nothing
                   EQ -> Just Zero
                   GT -> Just $ Succ $ maybe Zero id (integerToNat $ n - 1)
