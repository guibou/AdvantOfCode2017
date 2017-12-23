module Day18 where

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

-- 23h40
-- 00h51`
-- 02h07 it tooks me one hour to realise that JNZ uses > and not !=. Damned me

fileContent :: Text
fileContent = $(getFile)

data Register = Register Char
  deriving (Show, Ord, Eq)
data RegisterOrInt = ROI Register | ROIInt Int
  deriving (Show, Eq)

data Instr = Snd RegisterOrInt
           | Rcv Register
           | Set Register RegisterOrInt
           | Add Register RegisterOrInt
           | Mul Register RegisterOrInt
           | Mod Register RegisterOrInt
           | Jgz RegisterOrInt RegisterOrInt
  deriving (Show)

-- * Generics
parseInstr :: Parser Instr
parseInstr = choice
  [ parseUnOp "snd" Snd parseRegisterOrInt
  , parseBinOp "set" Set parseRegister parseRegisterOrInt
  , parseBinOp "add" Add parseRegister parseRegisterOrInt
  , parseBinOp "mul" Mul parseRegister parseRegisterOrInt
  , parseBinOp "mod" Mod parseRegister parseRegisterOrInt
  , parseUnOp "rcv" Rcv parseRegister
  , parseBinOp "jgz" Jgz parseRegisterOrInt parseRegisterOrInt
  ]

parseBinOp :: Text -> (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseBinOp s ctor a b = ctor <$> (string s >> char ' ' *> a) <*> (char ' ' *> b)

parseUnOp :: Text -> (a -> Instr) -> Parser a -> Parser Instr
parseUnOp s ctor a = ctor <$> (string s >> char ' ' *> a)

parseRegister :: Parser Register
parseRegister = Register <$> (oneOf ['a'..'z'])

parseRegisterOrInt :: Parser RegisterOrInt
parseRegisterOrInt = choice
  [ ROI <$> parseRegister
  , ROIInt <$> number
  ]

number :: Parser Int
number = do
  sign <- option (*1) ((*(-1)) <$ char '-')
  n <- decimal
  pure (sign n)

parseContent = unsafeParse (parseInstr `sepBy` char '\n')

-- * FIRST problem
evalInstr :: [Instr] -> Maybe Int
evalInstr instrL = go 0 (Nothing, Map.empty)
  where
    instr = V.fromList instrL
    go pc (freq, regs) = case instr V.!? pc of
      Nothing -> Nothing
      Just i -> case i of
        Set reg roi -> go (pc + 1) (freq, Map.insert reg (roiValue regs roi) regs)
        Add reg roi -> go (pc + 1) (freq, Map.insert reg (regValue regs reg + roiValue regs roi) regs)
        Mul reg roi -> go (pc + 1) (freq, Map.insert reg (regValue regs reg * roiValue regs roi) regs)
        Mod reg roi -> go (pc + 1) (freq, Map.insert reg (regValue regs reg `rem` roiValue regs roi) regs)
        Jgz reg roi ->
          let
            skip = roiValue regs roi
            test = roiValue regs reg
          in go (if test > 0 then pc + skip else (pc + 1)) (freq, regs)
        Snd roi -> go (pc + 1) (Just (roiValue regs roi), regs)
        Rcv reg ->
          let
            test = regValue regs reg
          in if test /= 0
             then freq
             else go (pc + 1) (freq, regs)

roiValue :: Map Register Int -> RegisterOrInt -> Int
roiValue _ (ROIInt i) = i
roiValue m (ROI r) = regValue m r

regValue m r = fromMaybe 0 (Map.lookup r m)

day :: [Instr] -> Maybe Int
day = evalInstr

-- * SECOND problem
data Op = Send Int Op
        | Recv (Int -> Op)
        | Done

run :: Int -> [Instr] -> Op
run pID instrL = go 0 (Map.singleton (Register 'p') pID)
  where
    instr = V.fromList instrL
    go pc regs = case instr V.!? pc of
      Nothing -> Done
      Just i -> case i of
        Set reg roi -> go (pc + 1) (Map.insert reg (roiValue regs roi) regs)
        Add reg roi -> go (pc + 1) (Map.insert reg (regValue regs reg + roiValue regs roi) regs)
        Mul reg roi -> go (pc + 1) (Map.insert reg (regValue regs reg * roiValue regs roi) regs)
        Mod reg roi -> go (pc + 1) (Map.insert reg (regValue regs reg `rem` roiValue regs roi) regs)
        Jgz reg roi ->
          let
            skip = roiValue regs roi
            test = roiValue regs reg
          in go (if test > 0 then pc + skip else (pc + 1)) regs
        Snd roi -> Send (roiValue regs roi) (go (pc + 1) regs)
        Rcv reg -> Recv (\x -> go (pc + 1) (Map.insert reg x regs))

day' :: [Instr] -> Int
day' instr = let
  process0 = run 0 instr
  process1 = run 1 instr
  in countSend process0 process1

countSend process0 process1 = go 0 (process0, process1) (Seq.empty)
  where
    go c ps sendP0 = case ps of
      (Send i0 p0', p1') -> go c (p0',p1') (sendP0 Seq.|> i0)
      (Recv contA, Send i p1') -> go (c + 1) (contA i, p1') sendP0
      (p0, Recv contB) -> case sendP0 of
        Seq.Empty -> c
        x Seq.:<| xs -> go c (p0, contB x) xs
      (Done, Send _ p1') -> go (c + 1) (Done, p1') sendP0
      (Recv _, Done) -> c
      (Done, Done) -> c



-- * Tests

testData :: Text
testData = [here|set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2|]

testData' :: Text
testData' = [here|snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day (parseContent testData) `shouldBe` Just 4
    it "of second star" $ do
      day' (parseContent testData') `shouldBe` 3
  describe "woks" $ do
    it "on first star" $ do
      day (parseContent fileContent) `shouldBe` Just 2951
    it "on second star" $ do
      day' (parseContent fileContent) `shouldBe` 7366
