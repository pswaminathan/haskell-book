module Addition where

import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
  testMultBy
  testDivBy
  testAddition
  runQc


testAddition :: IO ()
testAddition = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1::Integer) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2::Integer) + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)



runQc :: IO ()
runQc = quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

testDivBy :: IO ()
testDivBy = hspec $ do
  describe "divBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15::Integer) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 rem 2" $ do
      dividedBy (22::Integer) 5 `shouldBe` (4, 2)


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
      multBy (3::Integer) 4 `shouldBe` 12
    it "order of operands shouldn't matter" $ do
      multBy (3::Integer) 4 `shouldBe` (multBy 4 3)
    it "12 mult by -2 is -24" $ do
      multBy (12::Integer) (-2) `shouldBe` (-24)
    it "-12 mult by 2 is -24" $ do
      multBy (-12::Integer) 2 `shouldBe` (-24)
    it "negatives cancel out" $ do
      multBy (-12::Integer) (-2) `shouldBe` 24



sayHello :: IO ()
sayHello = putStrLn "hello!"
