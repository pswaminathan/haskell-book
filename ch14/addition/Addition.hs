module Addition where

import           Test.Hspec

main :: IO ()
main = do
  testMultBy
  testDivBy


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

testDivBy :: IO ()
testDivBy = hspec $ do
  describe "divBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 rem 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)


multBy :: (Num a, Ord a) => a -> a -> a
multBy a b
    | a < 0 && b < 0 = multBy (-a) (-b)
    | a < 0 = negate $ multBy (-a) b
    | b < 0 = negate $ multBy a (-b)
    | otherwise = go a b 0
        where go _ 0 z = z
              go x y z = go x (y - 1) (x + z)

testMultBy :: IO ()
testMultBy = hspec $ do
  describe "multBy" $ do
    it "3 mult by 4 is 12" $ do
      multBy 3 4 `shouldBe` 12
    it "order of operands shouldn't matter" $ do
      multBy 3 4 `shouldBe` (multBy 4 3)
    it "12 mult by -2 is -24" $ do
      multBy 12 (-2) `shouldBe` (-24)
    it "-12 mult by 2 is -24" $ do
      multBy (-12) 2 `shouldBe` (-24)
    it "negatives cancel out" $ do
      multBy (-12) (-2) `shouldBe` 24



sayHello :: IO ()
sayHello = putStrLn "hello!"
