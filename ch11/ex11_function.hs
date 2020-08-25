module Ex11Function where

data Quantum = Yes
    | No
    | Both
    deriving (Eq, Show)


-- either is a sum type, so its cardinality is
-- the sum of that of its inhabitants:
-- 3 + 3 = 6
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSums :: [Either Quantum Quantum]
quantSums = [Right Yes, Right No, Right Both, Left Yes, Left No, Left Both]

quantProds :: [(Quantum, Quantum)]
quantProds = [ (Yes, Yes)
             , (Yes, No)
             , (Yes, Both)
             , (No, Yes)
             , (No, No)
             , (No, Both)
             , (Both, Yes)
             , (Both, No)
             , (Both, Both)
             ]

convert :: Quantum -> Bool
convert Yes  = True
convert No   = True
convert Both = True

conv2 :: Quantum -> Bool
conv2 Yes  = False
conv2 No   = True
conv2 Both = True

conv3 :: Quantum -> Bool
conv3 Yes  = False
conv3 No   = False
conv3 Both = True

conv4 :: Quantum -> Bool
conv4 Yes  = False
conv4 No   = False
conv4 Both = False

conv5 :: Quantum -> Bool
conv5 Yes  = True
conv5 No   = False
conv5 Both = True

conv6 :: Quantum -> Bool
conv6 Yes = True
-- False
-- False


-- True
-- True
-- False

-- True
-- False
-- False
