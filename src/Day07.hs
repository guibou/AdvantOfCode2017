module Day07 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.Text as Text

-- * PARSING
prgName :: Parser Text
prgName = Text.pack <$> (some (oneOf ['a'..'z']))

parseProgram :: Parser (Text, (Int, [Text]))
parseProgram = do
  s <- lexeme prgName
  _ <- char '('
  d <- L.decimal
  _ <- lexeme (char ')')

  friends <- option [] $ do
    _ <- lexeme (string "->")
    (lexeme prgName) `sepBy` (string ", ")

  pure $ (s, (d, friends))

instructions :: Parser Data
instructions = Map.fromList <$> many (parseProgram)

parseContent :: Text -> Data
parseContent c = unsafeParse instructions c

type Data = Map Text (Int, [Text])

fileContent :: Data
fileContent = parseContent $(getFile)

-- * Generics

-- * FIRST problem
day :: Data -> Text
day tree = Set.elemAt 0 $ uncurry Set.difference $
           foldl' f (Set.empty, Set.empty) (Map.toList tree)
  where
    f (items, hasParent) (name, (_, childs)) = (Set.insert name items,
                                                Set.union hasParent (Set.fromList childs))

-- * SECOND problem
day' :: Data -> Maybe (Balance (Int, Int))
day' d = balance d (day d)

newtype Balance t = Balance (Either Int t)
  deriving (Show, Eq)
  deriving newtype (Functor, Applicative, Monad)

pattern Balanced :: t -> Balance t
pattern Balanced t = Balance (Right t)

pattern Unbalanced :: Int -> Balance t
pattern Unbalanced i = Balance (Left i)

-- | returns the Balance factor for a subtree
balance :: Data -> Text -> Maybe (Balance (Int, Int))
balance m = go
  where
    go :: Text -> Maybe (Balance (Int, Int))
    go pName = do
      (weight, childs) <- Map.lookup pName m
      childsGo <- mapM go childs
      pure $ do
        childsBalance <- sequence childsGo
        childSizes <- balanceFactor childsBalance

        Balanced (weight, weight + childSizes)

-- | Returns `Left balanceFactor` or `Right sumOfWeights`
-- >>> balanceFactor [(5, 15), (7, 15), (3, 10)]
-- >>> Unbalanced 8 -- (3 + 8 to go to 15)
-- >>> balanceFactor [(5, 15), (7, 15)]
-- >>> Balanced 30
balanceFactor :: [(Int, Int)] -> Balance Int
balanceFactor [] = Balanced 0
balanceFactor l = let
  lWeights = map snd l
  mi = minimum lWeights
  ma = maximum lWeights

  in if mi == ma
    then Balanced (sum lWeights)
    else let
    countMi = countItem mi lWeights
    countMa = countItem ma lWeights

    (uncorrect, correct) = if countMi > countMa
                           then (ma, mi)
                           else (mi, ma)

    (curWeight, a) = unsafeFromJust (find (\(_, w) -> w == uncorrect) l)
    newWeight = curWeight + (correct - a)
    in Unbalanced newWeight

-- * Tests

test :: IO ()
test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData `shouldBe` "tknk"
    it "of second star" $ do
      balance testData "ugml" `shouldBe` Just (Balanced (68, 251))
      balance testData "padx" `shouldBe` Just (Balanced (45, 243))
      balance testData "fwft" `shouldBe` Just (Balanced (72, 243))
      day' testData `shouldBe` Just (Unbalanced 60)
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "gmcrj"
    it "on second star" $ do
      day' fileContent `shouldBe` Just (Unbalanced 391)

-- 9h01: start
-- 9h15: first star
-- 10h13

-- 913: too high

testContent :: Text
testContent = [here|pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)|]

testData :: Data
testData = parseContent testContent
