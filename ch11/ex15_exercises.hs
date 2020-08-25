module ChapterExercises where

data Weekday = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    deriving (Eq, Enum, Ord, Show)

-- 1. a: Weekday is a type with five data constructors

-- 2: type below
f :: Weekday -> String
f Friday = "Miller Time"

-- 3. b: Must begin with a capital letter

g xs = xs !! (length xs - 1)

-- 4. c: Returns the final element of xs
