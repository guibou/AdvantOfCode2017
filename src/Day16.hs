module Day16 where

-- 21h36
-- 22h02
-- 22h30

import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

import Utils
import Data.List (findIndex)

import qualified Data.Map as Map

fileContent :: [Action]
fileContent = unsafeParse parser $(getFile)

parser = parserA `sepBy` char ','

data Action = Spin Int | Exchange Int Int | Partner Char Char
    deriving (Show)

parserA :: Parser Action
parserA = choice
  [ char 's' *> (Spin <$> decimal)
  , char 'x' *> (Exchange <$> decimal <*> (char '/' *> decimal))
  , char 'p' *> (Partner <$> anyChar <*> (char '/' *> anyChar))
  ]

-- * Generics


-- * FIRST problem
day :: [Action] -> [Char] -> [Char]
day actions prog = foldl' f prog actions

f prog (Spin x) = take (length prog) (drop (length prog - x) (cycle prog))
f prog (Partner a b) = map swp prog
  where swp x
          | x == a = b
          | x == b = a
          | otherwise = x
f prog (Exchange i j) = zipWith f [0..] prog
  where f idx c
          | idx == i = prog `unsafeIndex` j
          | idx == j = prog `unsafeIndex` i
          | otherwise = c

-- * SECOND problem
findRep actions prog =
  let
    items = iterate (day actions) prog

    go i p
      | p == prog = i
      | otherwise = go (i + 1) (day actions p)
  in go 1 (day actions prog)

day' actions prog = applyN (10 ^ 9 `mod` (findRep actions prog)) (day actions) prog

applyPerm perm prog = map (\idx -> prog `unsafeIndex` idx) perm

-- * Tests

testData = unsafeParse parser "s1,x3/4,pe/b"

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData ['a'..'e'] `shouldBe` "baedc"
  describe "works" $ do
    it "on first star" $ do
      day fileContent ['a'..'p'] `shouldBe` "olgejankfhbmpidc"
    it "on second star" $ do
      day' fileContent ['a'..'p'] `shouldBe` "gfabehpdojkcimnl"
