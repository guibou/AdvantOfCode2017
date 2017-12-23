module Day23 where

-- Start: 16h43
-- First star: 16h57

import Utils
import Day18 (parseBinOp, parseRegister, parseRegisterOrInt, RegisterOrInt(..), Register(..), roiValue, regValue)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Map as Map

import qualified Data.Vector as V

data Instr = Set Register RegisterOrInt
           | Mul Register RegisterOrInt
           | Sub Register RegisterOrInt
           | Jnz RegisterOrInt RegisterOrInt
           | Nop
           | Optimisation
  deriving (Show, Eq)

-- * Generics
parseInstr :: Parser Instr
parseInstr = choice
  [ parseBinOp "set" Set parseRegister parseRegisterOrInt
  , parseBinOp "sub" Sub parseRegister parseRegisterOrInt
  , parseBinOp "mul" Mul parseRegister parseRegisterOrInt
  , parseBinOp "jnz" Jnz parseRegisterOrInt parseRegisterOrInt
  ]

parseContent = unsafeParse (parseInstr `sepBy` char '\n')

fileContent :: [Instr]
fileContent = parseContent $(getFile)

-- * Generics
eval :: [Instr] -> Map Register Int -> (Int, Map Register Int)
eval instr' regs = go 0 0 regs
  where
    go pc !count regs = case instr V.!? pc of
      Nothing -> (count, regs)
      Just i -> case i of
        Set r roi -> go (pc + 1) count (Map.insert r (roiValue regs roi) regs)
        Mul r roi -> go (pc + 1) (count + 1) (Map.insert r (regValue regs r * roiValue regs roi) regs)
        Sub r roi -> go (pc + 1) count (Map.insert r (regValue regs r - roiValue regs roi) regs)
        Jnz roiTest roiOffset -> let
          pc' = if roiValue regs roiTest /= 0
                then pc + roiValue regs roiOffset
                else pc + 1
          in go pc' count regs
        Optimisation -> go (pc + 1) count (overrideF regs)
          where
            overrideF regs = let
              b = regs Map.! Register 'b'
              in Map.insert (Register 'f') (if isPrime b then 1 else 0) regs
        Nop -> go (pc + 1) count regs

    instr = V.fromList instr'

-- slow isPrime
isPrime :: Int -> Bool
isPrime n = not (any (\x -> n `mod` x == 0) [2..truncate (sqrt @Float (fromIntegral n))])

-- * FIRST problem
day :: [Instr] -> Int
day instr = fst $ eval instr Map.empty

-- * SECOND problem
day' :: [Instr] -> Maybe Int
day' instr = Map.lookup (Register 'h') $ snd $ eval (optimise instr) (Map.singleton (Register 'a') 1)

optimise :: [Instr] -> [Instr]
optimise [] = []
optimise l@(x:xs)
  | optimiseBlockA `isPrefixOf` l = optimisedBlockA ++ optimise (drop (length optimiseBlockA) l)
  | otherwise = x : optimise xs

optimiseBlockA = parseContent [here|
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
|]

optimisedBlockA = [
  Optimisation
  , Set (Register 'e') (ROI $ Register 'b')
  , Set (Register 'd') (ROI $ Register 'b')
  , Set (Register 'g') (ROIInt 0)
  ] ++ replicate (length optimiseBlockA - 4) Nop

{-
do
  e = 2

  f = ...
  e = b
  g = 0

  d += 1
  g = d
  g -= b
while(g != 0)

written variables: e, f, g, d

# e = b
# f = last special one...
# g = 0 = d - b
# d = b

-}

{-


set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13

    e = 2
        g = d
        g *= e
        g -= b
        if(g == 0)
            f = 0;
        e += 1
        g = e - b
    while(g != 0)
    d += 1
    g = d - b
while(g != 0)



f = 1
d = 2
    e = 2
        if(d * e == b)
            f = 0;
        e += 1
    while(e != b)
    d += 1
while(d != b)

-->
f = ?
e = b
d = b
g = 0

for d \in [2..b]
   for e in [2..b]
      any (d * e == b)

2 <= d <= b
2 <= e <= b

4 <= d * e <= b * b

-}
-- * Tests
test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3025
    it "on second star" $ do
      day' fileContent `shouldBe` Just 915
