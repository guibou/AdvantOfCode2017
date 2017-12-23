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
  deriving (Show)

-- * Generics
parseInstr :: Parser Instr
parseInstr = choice
  [ parseBinOp "set" Set parseRegister parseRegisterOrInt
  , parseBinOp "sub" Sub parseRegister parseRegisterOrInt
  , parseBinOp "mul" Mul parseRegister parseRegisterOrInt
  , parseBinOp "jnz" Jnz parseRegisterOrInt parseRegisterOrInt
  ]

fileContent :: [Instr]
fileContent = unsafeParse (parseInstr `sepBy` char '\n') $(getFile)

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

    instr = V.fromList instr'

-- * FIRST problem
day :: [Instr] -> Int
day instr = fst $ eval instr Map.empty

-- * SECOND problem
day' :: [Instr] -> Maybe Int
day' instr = Map.lookup (Register 'h') $ snd $ eval instr (Map.singleton (Register 'a') 1)

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3025
    it "on second star" $ do
      1 `shouldBe` 2
      -- day' fileContent `shouldBe` undefined
      --undefined `shouldBe` undefined
