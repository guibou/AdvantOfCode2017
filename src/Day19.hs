module Day19 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char

-- start : 23h35
-- first star: 00h12
-- second star: 00h16

import qualified Data.Matrix as M

data Direction = Horizontal | Vertical
  deriving (Show)

data Item = Empty | Cross | Dir Direction | Letter Char
  deriving (Show)

parseItem :: Parser Item
parseItem =
      Empty <$ char ' '
  <|> Cross <$ char '+'
  <|> Dir Horizontal <$ char '-'
  <|> Dir Vertical <$ char '|'
  <|> Letter <$> (oneOf ['A'..'Z'])

parseContent :: Text -> M.Matrix Item
parseContent t = M.fromLists (unsafeParse (some parseItem `sepBy` char '\n') t)

fileContent :: M.Matrix Item
fileContent = parseContent $(getFile)

-- * Generics
data Motion = North | West | South | East
  deriving (Show, Eq)

move :: Motion -> (Int, Int) -> (Int, Int)
move m (l, c) = case m of
  North -> (l - 1, c)
  South -> (l + 1, c)
  East -> (l, c + 1)
  West -> (l, c - 1)

deltaToDirection North = Vertical
deltaToDirection South = Vertical
deltaToDirection West = Horizontal
deltaToDirection East = Horizontal

walkGraph :: M.Matrix Item -> [Item]
walkGraph m = go (findStart m) South
  where
    go :: (Int, Int) -> Motion -> [Item]
    go p@(l, c) delta = let e = M.getElem l c m in case e of
          Empty -> []
          Cross -> let delta' = crossMove p delta
                   in e : go (move delta' p) delta'
          -- Any direction or letter: continue our path
          _ -> e : go (move delta p) delta

    crossMove p delta = case deltaToDirection delta of
      Vertical -> tryHorizontal
      Horizontal -> tryVertical
      where
        lookDirection d = fromMaybe Empty (M.safeGet l c m)
          where (l, c) = move d p

        tryHorizontal = case lookDirection East of
          Empty -> case lookDirection West of
            Empty -> panic "WTFA"
            _ -> West
          _ -> East
        tryVertical = case lookDirection North of
          Empty -> case lookDirection South of
            Empty -> panic "WTFB"
            _ -> South
          _ -> North

findStart :: M.Matrix Item -> (Int, Int)
findStart m = go (1, 1)
  where
    go current@(l, c) = case M.getElem l c m of
      Dir Vertical -> current
      _ -> go (l, c + 1)

-- * FIRST problem
day :: M.Matrix Item -> [Char]
day m = [x | Letter x <- walkGraph m]

-- * SECOND problem
day' :: M.Matrix Item -> Int
day' m = length (walkGraph m)

-- * Tests
display Empty = ' '
display Cross = '+'
display (Dir Horizontal) = '-'
display (Dir Vertical) = '|'
display (Letter c) = c

testData = parseContent $ [hereLit|     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ |]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData `shouldBe` "ABCDEF"
    it "of second star" $ do
      day' testData `shouldBe` 38
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "RYLONKEWB"
    it "on second star" $ do
      day' fileContent `shouldBe` 16016
