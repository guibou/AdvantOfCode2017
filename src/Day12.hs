module Day12 where

import Utils

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Data.UnionFind.ST

import qualified Data.Map as Map

fileContent :: [D]
fileContent = unsafeParse progs $(getFile)

type D = (Int, [Int])

parseLine :: Parser D
parseLine = do
  n <- L.decimal
  _ <- string " <-> "
  ns <- L.decimal `sepBy` string ", "

  pure (n, ns)

progs :: Parser [D]
progs = parseLine `sepBy` string "\n"

merge :: Map Int (Point s Int) -> D -> ST s ()
merge m (a, xs) = do
  let a' = m Map.! a
      xs' = map (m Map.!) xs

  mapM_ (union a') xs'

-- * Generics
getDescriptors :: [D] -> Map Int Int
getDescriptors l = runST $ do
  let ints = map fst l
  points <- mapM fresh ints

  let m = Map.fromList (zip ints points)

  mapM_ (merge m) l

  descs <- mapM descriptor points

  pure (Map.fromList $ zip ints descs)

-- * FIRST problem
day :: [D] -> Int
day l = countIf (==desc0) (Map.elems descriptors)
  where
    descriptors = getDescriptors l
    desc0 = descriptors Map.! 0

-- * SECOND problem
day' :: [D] -> Int
day' l = length (ordNub (Map.elems descriptors))
  where
    descriptors = getDescriptors l

-- * Tests
example :: [D]
example = unsafeParse progs [here|0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day example `shouldBe` 6
    it "of second star" $ do
      day' example `shouldBe` 2
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 378
    it "on second star" $ do
      day' fileContent `shouldBe` 204

-- 14h48
-- star1: 15h18 (at work, so busy by some other tasks ;)
-- star2: 15h18
