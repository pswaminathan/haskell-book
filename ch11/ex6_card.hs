module Card where

import           Data.Int

data BigSmall = Big Bool
    | Small Bool
    deriving (Eq, Show)

-- cardinality:
-- card (Big Bool) + card (Small Bool)
-- = 2 + 2 = 4


data NumberOrBool = Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

myNumba = Numba (-128)

-- cardinality:
-- card (Numba Int8) + card (BoolyBool Bool)
-- = 2^8 + 2 = 258


data QuantumBool = QuantumTrue
    | QuantumFalse
    | QuantumBoth
    deriving (Eq, Show)

-- cardinality of type: 3

data TwoQs = MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)

type TupTwoQs = (QuantumBool, QuantumBool)
