module Day3 where

import Utils
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Control.Monad (guard)

-- 11h18
-- 11h58: both stars
fileContent :: Int
fileContent = 277678

-- | Enumerate the relative motion to build the spiral
spiralSeq :: [(Int, Int)]
-- next,                      tr,     tl      , bl,        , br
spiralSeq = concatMap (\n -> concatMap unpackD [(1, 0), (0, - n + 1 + 1), (-n+1, 0), (0, n - 1), (n - 1, 0)]) [3, 5..]
  where unpackD (0, dy) = replicate (abs dy) (0, signum dy)
        unpackD (dx, 0) = replicate (abs dx) (signum dx, 0)

-- | Position of the succesive items of the spiral
spiral = scanl (\(x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0) spiralSeq
-- * Generics

-- * FIRST problem

-- | Nième item on the spiral
posN n = spiral !! n

-- | distance to the center
distanceM (x, y) = abs x + abs y

-- The problem count starts at 1 when my code starts at 0, hence -1
day i = distanceM (posN (i - 1))

-- * SECOND problem

-- | neighbors allocation
spiralSumKnownNeighbors :: [Int]
-- first item is skiped because that's a special case
spiralSumKnownNeighbors = 1: go (drop 1 spiral) (Map.singleton (0, 0) 1)
  where go (pos@(x, y):xs) m = value:go xs m'
          where
            voisins = do
              dx <- [-1..1]
              dy <- [-1..1]

              guard ((dx, dy) /= (0, 0))
              pure (Map.lookup (x + dx, y + dy) m)

            value = sum $ catMaybes voisins
            m' = Map.insert pos value m


day' input = find (>input) spiralSumKnownNeighbors

-- * Tests

test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day 1 `shouldBe` 0
      day 12 `shouldBe` 3
      day 23 `shouldBe` 2
      day 1024 `shouldBe` 31
  it "of second star" $ do
    take 11 spiralSumKnownNeighbors `shouldBe` [1,1,2,4,5,10,11,23,25,26,54]
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 475
    it "on second star" $ do
      day' fileContent `shouldBe` (Just 279138)
