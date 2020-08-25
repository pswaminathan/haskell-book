module Ex7Records where

jm = Person "julie" 108
ca = Person "chris" 16


data Person = Person
    { name :: String
    , age  :: Int
    }
    deriving (Eq, Show)


data Fiction = Fiction
    deriving Show
data Nonfiction = Nonfiction
    deriving Show

data BookType = FictionBook Fiction
    | NonfictionBook Nonfiction
    deriving Show

data SimpBookType = FictBook
    | NonfictBook
    deriving Show


type AuthorName = String

-- data Author = Author (AuthorName, BookType)
data Author = Fiction AuthorName
    | Nonfiction AuthorName
    deriving (Eq, Show)

data SimpAuthor = FictBook AuthorName
    | NonfictBook AuthorName
    deriving (Eq, Show)
