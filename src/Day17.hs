module Day17 where

import Utils

fileContent :: Int
fileContent = 328

-- 22h33
-- 22h57
-- 23h35

-- * Generics
data CircularBuffer = CircularBuffer [Int] [Int]
  deriving (Show)

initBuffer = CircularBuffer [0] []

oneStepForward (CircularBuffer [] xr) = oneStepForward (CircularBuffer (reverse xr) [])
oneStepForward (CircularBuffer (x:xs) xr) = CircularBuffer xs (x:xr)

insertBuffer x (CircularBuffer l xs) = CircularBuffer (x:l) xs

manyStepsForward i rot b = applyN (rot `mod` i) oneStepForward b

inserts items rot buffer = foldl' (\b i -> insertBuffer i (manyStepsForward i (rot + 1) b)) buffer items

-- * FIRST problem
day :: Int -> Int
day rots =
  let (CircularBuffer l l') = inserts [1..2017] rots initBuffer
  in (l ++ reverse l') `unsafeIndex` 1

-- * SECOND problem
day' :: Int -> Int -> Int
day' rot end = let (res, _, _, _) = go (0, 0, 0, 1)
               in res
  where
    go final@(!res, !pos0, !currentPos, !toInsert)
      | toInsert == (end + 1) = final
      | newPos < pos0 = go (res, (pos0 + 1), newPos + 1, (toInsert + 1))
      | newPos == pos0 = go (toInsert, pos0, newPos + 1, (toInsert + 1))
      | otherwise = go (res, pos0, newPos + 1, (toInsert + 1))
      where
        newPos = (currentPos + rot) `mod` (toInsert)


-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 3 `shouldBe` 638
    it "of second star" $ do
      day' 3 1 `shouldBe` 1
      day' 3 2 `shouldBe` 2
      day' 3 3 `shouldBe` 2
      day' 3 4 `shouldBe` 2
      day' 3 5 `shouldBe` 5
      day' 3 6 `shouldBe` 5
      day' 3 7 `shouldBe` 5
      day' 3 8 `shouldBe` 5
      day' 3 9 `shouldBe` 9
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1670
    it "on second star" $ do
      day' fileContent (50 * 10 ^ 6) `shouldBe` 2316253
