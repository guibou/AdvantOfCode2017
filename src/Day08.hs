module Day08 where

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as Text

import qualified Data.Map as Map

-- start: 8h56
-- 8h18: first star
-- 8h20: second star

{- I may have been quicker with:

- A less robust parser (lexeme are not needed)
- Instead of creating a data for Op, I should have directly stored the
  (Int -> Int -> Bool) function. This save the `evalOp` function
-}

fileContent :: Text
fileContent = $(getFile)

parseContent :: Text -> [Instruction]
parseContent s = unsafeParse parseProgram s
-- * EDSL

data Op = LessThan | GreaterThan | LTE | GTE | Equal | NEQ
  deriving (Show)

data Cond = Cond Text Op Int
  deriving (Show)

data Instruction = Instruction Text Instr Int Cond
  deriving (Show)

data Instr = Inc | Dec
  deriving Show

-- * Parser
parseRegister :: Parser Text
parseRegister = Text.pack <$> lexeme (many (oneOf ['a'..'z']))

parseInstruction :: Parser Instr
parseInstruction = (Inc <$ symbol "inc") <|> (Dec <$ symbol "dec")

parseNumber :: Parser Int
parseNumber = lexeme $ do
  mult <- option (*1) ((*(-1)) <$ char '-')
  i <- L.decimal

  pure (mult i)

parseCondition :: Parser Cond
parseCondition = do
  _ <- symbol "if"
  reg <- parseRegister
  op <- parseOp
  n <- parseNumber

  pure (Cond reg op n)

parseOp :: Parser Op
parseOp = choice [
  LTE <$ symbol "<=",
  GTE <$ symbol ">=",
  LessThan <$ symbol "<",
  GreaterThan <$ symbol ">",
  Equal <$ symbol "==",
  NEQ <$ symbol "!="
  ]

parseLine :: Parser Instruction
parseLine = do
  reg <- parseRegister
  i <- parseInstruction
  n <- parseNumber
  cond <- parseCondition

  pure (Instruction reg i n cond)

parseProgram :: Parser [Instruction]
parseProgram = many parseLine
-- * Generics

evalMax :: [Instruction] -> (Map Text Int, Int)
evalMax is = foldl' e' (Map.empty, 0) is
  where
    e' (m, currentMax) (Instruction reg instr val cond)
      | testCondition cond m = let m' = applyInstr reg instr val m
                               in (m', max currentMax (maxRegister m'))
      | otherwise = (m, currentMax)

maxRegister :: Map Text Int -> Int
maxRegister = maximum . Map.elems

testCondition :: Cond -> Map Text Int -> Bool
testCondition (Cond reg op i) m = evalOp op (fromMaybe 0 (Map.lookup reg m)) i

evalOp :: (Ord a, Eq a) => Op -> (a -> a -> Bool)
evalOp = \case
  LTE -> (<=)
  GreaterThan -> (>)
  LessThan -> (<)
  GTE -> (>=)
  Equal -> (==)
  NEQ -> (/=)

applyInstr :: Text -> Instr -> Int -> Map Text Int -> Map Text Int
applyInstr reg instr i m = Map.insertWith (+) reg (value instr) m
  where value Inc = i
        value Dec = -i

-- * FIRST problem
day :: Text -> Int
day t = maxRegister (fst (evalMax (parseContent t)))

-- * SECOND problem
day' :: Text -> Int
day' t = snd (evalMax (parseContent t))

-- * Tests

testString :: Text
testString = [here|b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testString `shouldBe` 1
    it "of second star" $ do
      day testString `shouldBe` 1
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4416
    it "on second star" $ do
      day' fileContent `shouldBe` 5199
