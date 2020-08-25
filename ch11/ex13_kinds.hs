module Ex13Kinds where

data Silly a b c d = MkSilly a b c d
    deriving Show


data List a = Nil
    | Cons a (List a)
    deriving Show

instance Eq a => Eq (List a) where
  (==) Nil Nil                   = True
  (==) (Cons x Nil) (Cons y Nil) = x == y
  (==) (Cons x xs) (Cons y ys)   = x == y && xs == ys
  (==) _ _                       = False


