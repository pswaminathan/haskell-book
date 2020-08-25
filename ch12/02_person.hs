module Person where

type Name = String
type Age = Integer

data Person = Person
    { name :: Name
    , age  :: Age
    }
    deriving (Show, Eq)


mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name == "" = Nothing
  | age < 0 = Nothing
  | otherwise = Just $ Person { name = name, age = age }

