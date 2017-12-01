module Day1 where

import Utils

import qualified Data.Vector as V

fileContent = $(getFile)

-- * Generics

val i = ord i - ord '0'

go (V.fromList -> v) offset = V.sum (V.map f (zipIndex v))
  where
    l = V.length v
    f (i, cur)
      | cur == (v V.! ((i + offset) `mod` l)) = val cur
      | otherwise = 0


-- * FIRST problem
day code = go code 1

-- * SECOND problem
day' code = go code (length code `div` 2)

-- * Tests

test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day "1122" `shouldBe` 3
    it "of second star" $ do
      day' "1212" `shouldBe` 6
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1228
    it "on second star" $ do
      day' fileContent `shouldBe` 1238
