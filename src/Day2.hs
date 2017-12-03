module Day2 where

import Utils
import Data.List (find, sort)

fileContent = $(getFile)

-- * Generics
parseContent f = map (map read . words) (lines f)

checksum1 l = maximum l - minimum l

checksum f d = sum (map f d)

checksum2 l = go (sort l)
  where
    go [] = error "WTF"
    go (x:xs) = case find (\i -> i `mod` x == 0) xs of
          Just i -> i `div` x
          Nothing -> checksum2 xs

-- * FIRST problem
day input = checksum checksum1 (parseContent input)

-- * SECOND problem
day' input = checksum checksum2 (parseContent input)

-- * Tests

test = hspec $ do
--  describe "simple examples" $ do
--    it "of first star" $ do
--      day "" `shouldBe` 0
--    it "of second star" $ do
--      day' "" `shouldBe` 0
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 36766
    it "on second star" $ do
      day' fileContent `shouldBe` 261
