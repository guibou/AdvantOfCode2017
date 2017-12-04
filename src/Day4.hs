module Day4 where

import Utils
import Data.List (sort, nub)

fileContent = $(getFile)

-- * Generics
parseContent s = map words (lines s)

countPassphares isPassphrase s = countIf isPassphrase s

-- * FIRST problem
-- `nub` is slow, but who cares ;)
day = countPassphares (\v -> length (nub v) == length v)

-- * SECOND problem
day' = countPassphares (\v -> length (nub (map sort v)) == length v)

-- * Tests

test = hspec $ do
 describe "woks" $ do
   it "on first star" $ do
     day (parseContent fileContent) `shouldBe` 325
   it "on second star" $ do
     day' (parseContent fileContent) `shouldBe` 119

-- 9h13
-- 9h14
-- 9h15
