module Day13 where

import Text.Megaparsec.Char
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Utils
import Data.List (findIndex)

{-
I initially represented the stack of Layer with a dedicated type, hand
coding the movement.

However, it exists an analytic solution to compute the position at
time t, so I implemented it in `f`.
-}

fileContent :: [Int]
fileContent = toLayers $ parseLayers $(getFile)

parseLayers :: Text -> [(Int, Int)]
parseLayers s = unsafeParse (p `sepBy` (char '\n')) s
  where p = do
          i <- L.decimal
          _ <- string ": "
          v <- L.decimal

          pure (i, v)

toLayers :: [(Int, Int)] -> [Int]
toLayers l = go 0 l
  where
    go _ [] = []
    go idx l@((idx', size):xs)
      | idx == idx' = size : go (idx + 1) xs
      | otherwise = 0 : go (idx + 1) l

-- * Generics
scannerPos 0 _ = Nothing
scannerPos range x
  | mDoubleRange >= range = Just (r2m2 -  mDoubleRange)
  | otherwise = Just mDoubleRange
  where
    r2m2 = 2 * range - 2
    mDoubleRange = x `mod` r2m2

process :: [Int] -> Int -> [(Int, Int)]
process l dT = go 0 l
  where
    go _ [] = []
    go position (x:xs)
      | scannerPos x (dT + position) == Just 0 = (position, x) : go (position + 1) xs
      | otherwise = go (position + 1) xs

-- * FIRST problem
day :: [Int] -> Int
day t = sum (map (uncurry (*)) $ process t 0)

-- * SECOND problem
day' :: [Int] -> Int
day' l = let Just i = findIndex (null.process l) [0..]
         in i

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData `shouldBe` 24
    it "of second star" $ do
      day' testData `shouldBe` 10
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1876
    it "on second star" $ do
      day' fileContent `shouldBe` 3964778

-- start: 9h36
-- first star: 10h04
-- second star: 10h09

testData :: [Int]
testData = toLayers $ parseLayers $ [here|0: 3
1: 2
4: 4
6: 4|]
