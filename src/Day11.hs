module Day11 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map

data Direction = N | NE | SE | S | SW | NW
  deriving (Show, Eq, Ord)

fileContent :: [Direction]
fileContent = map toDir (Text.splitOn "," $(getFile))

toDir :: Text -> Direction
toDir "n" = N
toDir "s" = S
toDir "ne" = NE
toDir "nw" = NW
toDir "se" = SE
toDir "sw" = SW
toDir _ = panic "WTF"

-- * Generics
type Counter t = Map t Int

counter :: Ord t => [t] -> Counter t
counter s = Map.fromListWith (+) (map (,1) s)

addToCounter :: Ord t => t -> Counter t -> Counter t
addToCounter x m = Map.insertWith (+) x 1 m

-- List of simplification patterns
patterns :: [(Counter Direction, Counter Direction)]
patterns = map (\(a, b) -> (counter a, counter b)) $ [
  ([NE, N,NW], [N, N])
  ,([SE, S, SW], [S, S])
  ,([NE, S], [SE])
  ,([NW, S], [SW])
  ,([SE, N], [NE])
  ,([SW, N], [NW])
  ,([NE, NW], [N])
  ,([SE, SW], [S])
  ,([N, S], [])
  ,([NW, SE], [])
  ,([NE, SW], [])
  ]

matchPattern :: Counter Direction -> Counter Direction -> Bool
matchPattern pat m = all (\(k, v) -> fromMaybe 0 (Map.lookup k m) >= v) (Map.toList pat)

killPattern :: Counter Direction -> (Counter Direction, Counter Direction) -> Counter Direction
killPattern m (pat,repl)
  | matchPattern pat m = Map.unionWith (+) repl (Map.unionWith (-) m pat)
  | otherwise = m

simplify :: Counter Direction -> Counter Direction
simplify m
  | m' == m = m
  | otherwise = simplify m'
  where
    m' = foldl' killPattern m patterns

-- * FIRST problem
day :: [Direction] -> Int
day d = sum (simplify (counter d))

-- * SECOND problem
day' :: [Direction] -> Int
day' d = maximum (map sum steps)
  where
    steps = scanl (\m x -> simplify (addToCounter x m)) Map.empty d

-- * Tests

test :: IO ()
test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day [NE, NE, NE] `shouldBe` 3
      day [NE, NE, SW, SW] `shouldBe` 0
      day [NE, NE, S, S] `shouldBe` 2
      day [SE, SW, SE, SW, SW] `shouldBe` 3
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 643
    it "on second star" $ do
      day' fileContent `shouldBe` 1471

-- start: 9h13
-- first star : 9h54
-- second (quadratic solution) star : 9h55
-- second (non-quadratic solution): 10h09
