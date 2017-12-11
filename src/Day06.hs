module Day06 where

import qualified Data.Vector as V
import qualified Data.Map as Map

import Utils
-- 7h57: reading the document (with two childs on my laps)
-- 8h15 -- first star, going to school
-- 8h56 back from school
-- 9h00 once I correctly understood the second star: got it
fileContent :: V.Vector Int
fileContent = V.fromList (unsafeRead1D $(getFile))

-- * Generics

-- | A memory reallocation routine
step :: V.Vector Int -> V.Vector Int
step v =
  let
    -- find the maximum value and its index
    mIdx = V.maxIndex v
    value = v V.! mIdx

    -- remove it from the initial vector
    zeroV = v V.// [(mIdx, 0)]

    -- the list of position to update
    updateList = take value $ map (`mod`(V.length v)) [mIdx + 1 .. ]
  in V.accum (+) zeroV (map (,1) updateList)

-- | Infinite memory reallocation routine
steps :: V.Vector Int -> [V.Vector Int]
steps v = iterate step v

-- | returns (number of reallocation until loop, length of loop)
loopDetection :: V.Vector Int -> (Int, Int)
loopDetection = go Map.empty 0
  where
    go m !c v = case Map.lookup v m of
      Just c' -> (c, c - c')
      Nothing -> go (Map.insert v c m) (c + 1) (step v)

-- * FIRST problem

-- | Find the number of reallocation steps before a loop is detected
day :: V.Vector Int -> Int
day = fst . loopDetection

-- * SECOND problem

-- | Length of the infinite loop
day' :: V.Vector Int -> Int
day' = snd . loopDetection

-- * Tests
test :: IO ()
test = hspec $ do
  describe "simple examples" $ do
    let ex = V.fromList [0,2,7,0]
    it "of first star" $ do
      day ex `shouldBe` 5
    it "of second star" $ do
      day' ex `shouldBe` 4
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 5042
    it "on second star" $ do
      day' fileContent `shouldBe` 1086
