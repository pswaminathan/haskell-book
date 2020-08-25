module Ex12Quad where

data Quad = One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = Right One
-- eQuad's type is Either Quad Quad
-- Either is a sum type, so its cardinality
-- is the sum of its habitants' cardinalities.
-- Quad is a sum type of four nullary constructors,
-- so its cardinality is four.
-- Thus Either Quad Quad is:
-- 4 + 4 = 8

prodQuad :: (Quad, Quad)
prodQuad = (One, Two) -- Fish
-- prodQuad is a product type of
-- two 4s. Thus:
-- 4 * 4 = 16

funcQuad :: Quad -> Quad
funcQuad One = Two
-- funcQuad is an exponent type:
-- card (a -> b) = b ^ a
-- 4 ^ 4 = 256

prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, False, True)
-- Product type, so it's the product
-- of all arguments:
-- 2 * 2 * 2 = 8

gTwo :: Bool -> Bool -> Bool
gTwo True False  = True
gTwo True True   = False
gTwo False True  = True
gTwo False False = False
-- gTwo is an exponenti:
-- 2 ^ 2 ^ 2 = 16

fTwo :: Bool -> Quad -> Quad
fTwo True One = Two
-- Exponent type:
-- 4 ^ 4 ^ 2 = 4 ^ 8 = 65536
