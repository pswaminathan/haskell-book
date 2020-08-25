module Ch11Dogs where

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a
    | Mastiff a
    deriving (Eq, Show)

{-
  1. Doggies is a type constructor
  2. * -> *
  3. *
  4. (Num a) => Doggies a
  5. Doggies Integer
  6. Doggies String
  7. It is both: which it is depends on the context in which it is used
  8. The type of DogueDeBordeaux is doge -> DogueDeBordeaux doge
  9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux String
-}

