module Day25 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Map as Map

-- start: 9h54
-- first star: 10h27

-- There is too much parsing involved here. Seriously, this may had
-- been really quick using a scanf or regex

data MachineState = MachineState Char
  deriving (Show, Ord, Eq)

type Program = ((MachineState, Int), Map MachineState (Block, Block))

type Block = (Bool, Direction, MachineState)

data Direction = ToLeft | ToRight
  deriving (Show)

fileContent :: Program
fileContent = parseContent $(getFile)

parseHeader :: Parser (MachineState, Int)
parseHeader = do
  _ <- string "Begin in state "
  statBegin <- parseMachineState
  _ <- string ".\n"
  _ <- string "Perform a diagnostic checksum after "
  d <- decimal
  _ <- string " steps.\n"
  pure (statBegin, d)

parseMachineState :: Parser MachineState
parseMachineState = MachineState <$> oneOf ['A'..'Z']

parseSubBlock :: Text -> Parser Block
parseSubBlock value = do
  _ <- string $ "  If the current value is " <> value <> ":\n"
  _ <- string "    - Write the value "
  v <- parseBool
  _ <- string ".\n    - Move one slot to the "
  dir <- parseDirection
  _ <- string ".\n    - Continue with state "
  nextState <- parseMachineState
  _ <- string "."

  pure (v, dir, nextState)

parseBool :: Parser Bool
parseBool = False <$ char '0' <|> True <$ char '1'

parseDirection :: Parser Direction
parseDirection = ToLeft <$ string "left" <|> ToRight <$ string "right"

parseBlock :: Parser (MachineState, (Block, Block))
parseBlock = do
  _ <- string "In state "
  statName <- parseMachineState
  _ <- string ":\n"
  block0 <- parseSubBlock "0"
  _ <- char '\n'
  block1 <- parseSubBlock "1"

  pure (statName, (block0, block1))

parseProgram :: Parser Program
parseProgram = do
  header <- parseHeader
  _ <- string "\n"
  blocks <- parseBlock `sepBy` string "\n\n"
  pure (header, Map.fromList blocks)

parseContent :: Text -> Program
parseContent = unsafeParse parseProgram

-- * Generics
-- I should definintly create a library for this Tape, I'm using it too often ;)

data Tape = Tape [Bool] [Bool]
          deriving (Show)

initTape :: Tape
initTape = Tape [False] []

toLeft :: Tape -> Tape
toLeft (Tape l []) = Tape (False:l) []
toLeft (Tape l (x:xs)) = Tape (x:l) xs

toRight :: Tape -> Tape
toRight (Tape [] r) = Tape [] r
toRight (Tape (x:xs) r) = Tape xs (x:r)

move :: Direction -> Tape -> Tape
move ToLeft = toLeft
move ToRight = toRight

write :: Bool -> Tape -> Tape
write v (Tape (_:xs) r) = Tape (v:xs) r
write v (Tape [] r) = Tape [v] r

read :: Tape -> Bool
read (Tape (v:_) _) = v
read (Tape [] _) = False

-- * FIRST problem
run :: Program -> Tape
run ((startState, nIters), prog) = go 0 startState initTape
  where
    go currentIter currentState currentTape
      | currentIter == nIters = currentTape
      | otherwise = let
          (blockFalse, blockTrue) = prog Map.! currentState
          currentValue = read currentTape
          (toWrite, toMove, nextState) = if currentValue then blockTrue else blockFalse
        in go (currentIter + 1) nextState (move toMove (write toWrite currentTape))

day :: Program -> Int
day prog = let (Tape a b) = run prog
           in (countIf (==True) (a ++ b))

-- * Tests

testData :: Program
testData = parseContent $ [here|
Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData `shouldBe` 3
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2846
