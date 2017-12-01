module DayX where

import Utils

fileContent = $(getFile)

-- * Generics


-- * FIRST problem
day code = undefined

-- * SECOND problem
day' code = undefined

-- * Tests

test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day "" `shouldBe` undefined
    it "of second star" $ do
      day' "" `shouldBe` undefined
--  describe "woks" $ do
--    it "on first star" $ do
--      day fileContent `shouldBe` 1228
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238
