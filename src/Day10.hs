module Day10 where

import Utils
import Data.Hex
import Data.Char
import qualified Data.Text as Text

fileContent :: Text
fileContent = "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"

-- * Generics

-- yeah, I called it bob... Because we all need a bob friend
data Bob = Bob [Int] Int Int
  deriving (Show)

init :: Int -> Bob
init i = Bob [0..i] 0 0

step len (Bob l skipSize offset) = Bob newL (skipSize + 1) newOffset
  where transL = reverse (take len l) ++ drop len l
        newL = take (length l) (drop (len + skipSize) (cycle transL))
        newOffset = offset + len + skipSize

foldSteps steps bob = foldl' (flip step) bob steps

normalizeBob (Bob l _ offset) = take (length l) $ drop move (cycle l)
  where move = length l - (offset `mod` (length l))

-- * FIRST problem
day :: [Int] -> Int -> Int
day steps ini = case normalizeBob (foldSteps steps (init ini)) of
  (a:b:_) -> a * b
  _ -> panic "WTF BBQ"

-- * SECOND problem
postFix = [17, 31, 73, 47, 23]

postFixedSeq :: Text -> [Int]
postFixedSeq s = (map ord (toS s)) ++ postFix

sparseHash :: Text -> Int -> [Int]
sparseHash steps ini = normalizeBob (applyN 64 (foldSteps (postFixedSeq steps)) (init ini))

denseHash :: [Int] -> [Int]
denseHash sparseOne = map (\l -> foldl' xor 0 l) (chunksOf 16 sparseOne)

toHex c = map toLower (hex [chr c])

knotHash :: [Int] -> Text
knotHash l = toS (concatMap toHex l)

day' :: Text -> Text
day' s = knotHash (denseHash (sparseHash s 255))

-- * Tests

exampleLengths = [3,4,1,5]
exampleI = 4

test :: IO ()
test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      normalizeBob (step 3 (init exampleI)) `shouldBe` [2, 1, 0, 3, 4]
      normalizeBob (foldSteps exampleLengths (init exampleI)) `shouldBe` [3, 4, 2, 1, 0]
      day exampleLengths 4 `shouldBe` 12
    it "of second star" $ do
      postFixedSeq "1,2,3" `shouldBe` [49,44,50,44,51,17,31,73,47,23]
      day' "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      day' "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      day' "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      day' "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
  describe "works" $ do
    it "on first star" $ do
      day (map unsafeRead (Text.splitOn "," fileContent)) 255 `shouldBe` 9656
    it "on second star" $ do
      day' fileContent `shouldBe` "20b7b54c92bf73cf3e5631458a715149"

-- start : 10:59
-- first : 11:22
-- second: 11:51 (But I had to fed my child ;))
