module Day15 where

import Utils
import Data.Bits

-- start: 6:04 (train was late ;()
-- first; 6h12
-- second: 6h20 (train power died, need to reboot computer ;()
-- * Generics
nextValue factor i = (i * factor) `rem` 2147483647

nextValueA = nextValue 16807
nextValueB = nextValue 48271

filterMod m = filter (\x -> x `mod` m == 0)

startA = 722
startB = 354

match (a, b) = a `mod` 65536 == b `mod` 65536

lengthMatch n l = length (filter match (take n l))

itA = iterate nextValueA
itB = iterate nextValueB

-- * FIRST problem
day :: Int
day = lengthMatch (40 * 10 ^ 6) (zip (itA startA) (itB startB))

-- * SECOND problem
day' ::Int
day' = lengthMatch (5 * 10 ^ 6) (zip (filterMod 4 $ itA startA) (filterMod 8 $ itB startB))

-- * Tests

test :: IO ()
test = hspec $ do
  describe "works" $ do
    it "on first star" $ do
      day `shouldBe` 612
    it "on second star" $ do
      day' `shouldBe` 285
