module Day5 where

import Utils

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

import Control.Monad.ST (runST)

-- 9h58
-- 9h37 (Seriously) (Python solution)

{-
I had issues with that one:

- Haskell Vector mutable are difficult to use, I don't think I can do it with an imutable one
- I did not understand the rule of star 2
- I wrongly answer with a leading coma, feezing the form for a minute
- I wrongly copy pasted the example.
- Stack overflow ! In Haskell ? ;)
I'm stupid ;)

- I finaly wrote the code in python in 1 minute and got the result... 29717847

- SPACE leak, I had a space leak in my accumulator (steps)... I'm a noob ;)
- Type signature, less polymorphic -> Int instead of Integer -> BOOST

-}

fileContent :: [Int]
fileContent = map read (lines $(getFile))

-- * Generics
{-# INLINE go #-}
go :: (Int -> Int) -> [Int] -> Int
go f content = runST $ do
  v <- V.unsafeThaw (V.fromList content)
  go' 0 0 v
  where
    go' offset !steps v
      | offset < 0 || offset >= VM.length v = pure steps
      | otherwise = do
          value <- VM.unsafeRead v offset
          VM.unsafeWrite v offset (f value)
          go' (offset + value) (steps + 1) v


-- * FIRST problem
day :: [Int] -> Int
day = go (+1)

-- * SECOND problem
day' :: [Int] -> Int
day' = go (\o -> if o >= 3 then o - 1 else o + 1)

-- * Tests

test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day [0, 3, 0, 1, -3] `shouldBe` 5
    it "of second star" $ do
      day' [0, 3, 0, 1, -3] `shouldBe` 10
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 381680
    it "on second star" $ do
      day' fileContent `shouldBe` 29717847
