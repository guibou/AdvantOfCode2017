module Day1 where

import Utils

fileContent = $(getFile)

-- * Generics

val i = ord i - ord '0'

go l offset = sum (map f (zip l l'))
  where
    l' = drop offset (cycle l)
    f (i, i')
      | i == i' = val i
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
