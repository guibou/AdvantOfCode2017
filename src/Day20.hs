{-# LANGUAGE DataKinds, KindSignatures #-}
module Day20 where

-- start: 18h11
-- firststar: 18h27 (bruteforce)
-- secondstar: 18h33 (bruteforce)

import Utils
import Day18 (number)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Map as Map

data Mode = Position | Velocity | Acceleration

data Triple (t :: Mode) = Triple Int Int Int
  deriving (Show, Ord, Eq)

data Point = Point (Triple 'Position) (Triple 'Velocity) (Triple 'Acceleration)
  deriving (Show, Ord, Eq)



fileContent :: [Point]
fileContent = parseContent $(getFile)

parseTriple :: Parser (Triple t)
parseTriple = do
  _ <- char '<'
  [a, b, c] <- number `sepBy` char ','
  _ <- char '>'

  pure $ Triple a b c

parseLine :: Parser Point
parseLine = do
  _ <- string "p="
  p <- parseTriple
  _ <- string ", v="
  v <- parseTriple
  _ <- string ", a="
  a <- parseTriple
  pure $ Point p v a

parseContent = unsafeParse (parseLine `sepBy` char '\n')

-- * Generics
tick :: Point -> Point
tick (Point p v a) = let
  v' = v .+. a
  p' = p .+. v'
  in Point p' v' a


(Triple x y z) .+. (Triple x' y' z') = Triple (x + x') (y + y') (z + z')

distance (Triple x y z) = abs x + abs y + abs z

-- * FIRST problem
day :: Int -> [Point] -> Int
day n pts = let
  pts' = zip (map (\(Point p _ _) -> distance p) $ applyN n (map tick) pts) [0..]
  in snd (minimum pts')

-- * SECOND problem
removeColliding :: [Point] -> [Point]
removeColliding pts = let
  m = Map.fromListWith (++) (map (\p@(Point pos _ _) -> (pos, [p])) pts)
  in [x | (_, [x]) <- Map.toList m]

day' :: Int -> [Point] -> Int
day' n pts = let
  pts' = map (\(Point p _ _) -> distance p) $ applyN n ((map tick) . removeColliding) pts
  in length pts'

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day 1000 fileContent `shouldBe` 119
    it "on second star" $ do
      day' 100 fileContent `shouldBe` 471
