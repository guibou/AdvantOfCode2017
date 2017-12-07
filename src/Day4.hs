module Day4 where

import Utils
import qualified Data.Text as Text

fileContent = $(getFile)

-- * Generics
parseContent s = map ((map Text.unpack) . Text.words) (Text.lines s)

countPassphares isPassphrase s = countIf isPassphrase s

-- * FIRST problem
day = countPassphares (\v -> length (ordNub v) == length v)

-- * SECOND problem
day' = countPassphares (\v -> length (ordNub (map sort v)) == length v)

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
