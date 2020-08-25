module EitherPerson where

type Name = String
type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data Person = Person
    { name :: Name
    , age  :: Age
    }
    deriving (Show, Eq)

data PersonInvalid = NameEmpty
    | AgeTooLow
    deriving (Eq, Show)

ageOk :: Age -> ValidatePerson Age
ageOk age = case age < 0 of
              True  -> Left [AgeTooLow]
              False -> Right age

nameOk :: Name -> ValidatePerson Name
nameOk name = case name == "" of
                True  -> Left [NameEmpty]
                False -> Right name

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name == "" = Left NameEmpty
  | age < 0    = Left AgeTooLow
  | otherwise  = Right $ Person name age


mkValidPerson :: Name
              -> Age
              -> ValidatePerson Person
mkValidPerson name age =
  mkValidPerson' (nameOk name) (ageOk age)
    where mkValidPerson' :: ValidatePerson Name
                         -> ValidatePerson Age
                         -> ValidatePerson Person
          mkValidPerson' (Right n) (Right a)          = Right $ Person n a
          mkValidPerson' (Left badName) (Left badAge) = Left $ badName ++ badAge
          mkValidPerson' (Left badName) _             = Left badName
          mkValidPerson' _ (Left badAge)              = Left badAge



{-
  Later in the book, we'll be able to replace mkValidPerson with:
  mkPerson
    :: Name
    -> Age
    -> Validation [PersonInvalid] Person
  mkPerson name age =
    liftA2
      Person (nameOk name) (ageOk age)
-}
