module Day03 where

import Utils
import qualified Data.Map as Map

-- 11h18
-- 11h58: both stars
fileContent :: Int
fileContent = 277678

data Direction = Horizontal | Vertical
data Move = Move Direction Int

-- | Enumerate the relative motion to build the spiral
spiralSeq :: [(Int, Int)]
-- next,                      tr,     tl      , bl,        , br
spiralSeq = concatMap (\n -> concatMap unpackD [
                          Move Horizontal 1,
                          Move Vertical (-n + 2),
                          Move Horizontal (-n + 1),
                          Move Vertical (n - 1),
                          Move Horizontal (n - 1)]) [3, 5..]
  where unpackD (Move dir c) = replicate (abs c) (getDir dir (signum c))
        getDir Horizontal c = (c, 0)
        getDir Vertical c = (0, c)

-- | Position of the succesive items of the spiral
spiral = scanl (\(x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0) spiralSeq
-- * Generics

-- * FIRST problem

-- | Nième item on the spiral
posN n = spiral `unsafeIndex` n

-- | distance to the center
distanceM (x, y) = abs x + abs y

-- The problem count starts at 1 when my code starts at 0, hence -1
day i = distanceM (posN (i - 1))

-- * SECOND problem

-- | neighbors allocation
spiralSumKnownNeighbors :: [Int]
-- first item is skiped because that's a special case
spiralSumKnownNeighbors = map fst (scanl f (1, Map.singleton (0, 0) 1) (drop 1 spiral))
  where
    f (_, m) pos@(x, y) = (value, m')
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

test = do
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
