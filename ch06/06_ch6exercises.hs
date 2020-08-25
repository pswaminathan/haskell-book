module Ch6Exercises where

-- 1
data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- 2
data Mood = Blah
    | Woot
    deriving (Eq, Show, Ord)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
               else x


-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "Dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- type checks
data Rocks = Rocks String
    deriving (Eq, Show, Ord)

data Yeah = Yeah Bool
    deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah
    deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)
-- will not type check because the data constructors

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- will not type check because no Ord
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
