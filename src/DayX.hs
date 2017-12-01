module DayX where

import Utils

fileContent = $(getFile)

-- * Generics


-- * FIRST problem
day = undefined

-- * SECOND problem
day' = undefined

-- * Tests

test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day "" `shouldBe` 0
    it "of second star" $ do
      day' "" `shouldBe` 0
--  describe "woks" $ do
--    it "on first star" $ do
--      day fileContent `shouldBe` 1228
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238
