module Ch6Numberish where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a


-- pretend newtype is data for now
newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65


newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1989


sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where intOfA = toNumber a
        intOfA' = toNumber a'
        summed = intOfA + intOfA'


add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
     then x + y
  else x

addWeird2 :: (Eq a, Num a) => a -> a -> a
addWeird2 x y =
  if x == y
     then x + y
  else x
