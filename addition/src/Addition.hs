module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denim = go num denim 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply a b = go a b 0
  where
    go 0 _ total = total
    go n m total = go (n-1) b (total + m)

-- sampling QuickCheck arbitrary generators:
--   sample $ (arbitrary :: Gen (Char, String, Int))

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      let one = 1 :: Integer
      (one + one > one) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      let two = 2 :: Integer
      two + two `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      let fifteen = 15 :: Integer
      fifteen `dividedBy` 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      let twentyTwo = 22 :: Integer
      twentyTwo `dividedBy` 5 `shouldBe` (4, 2)
    it "10 times 5 is 50" $ do
      let ten = 10 :: Integer
      ten `multiply` 5 `shouldBe` 50
    it "10 times 0 is 0" $ do
      let ten = 10 :: Integer
      ten `multiply` 0 `shouldBe` 0
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

