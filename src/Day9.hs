module Day9 where

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec

fileContent :: Text
fileContent = $(getFile)

-- * Parsing

data Group = Group [Group]
           | Garbage [Char]
           deriving (Show)

garbageChar :: Parser [Char]
garbageChar = escapedChar <|> aChar
  where
    escapedChar = [] <$ (char '!' *> anyChar)
    aChar = (:[]) <$> notChar '>'


parseGroupOrGarbage = parseGarbage <|> parseGroup
parseGarbage = Garbage . mconcat <$> (char '<' *> many garbageChar <* char '>')
parseGroup = Group <$> (char '{' *> parseGroupOrGarbage `sepBy` (char ',') <* char '}')
parseContent s = unsafeParse parseGroup s

-- * FIRST problem

countGroup :: Group -> Int
countGroup g = go 1 g
  where
    go currentScore (Group xs) = currentScore + sum (map (go (currentScore + 1)) xs)
    go _ (Garbage _) = 0

countGarbage :: Group -> Int
countGarbage (Garbage s) = length s
countGarbage (Group xs) = sum (map countGarbage xs)

day :: Text -> Int
day = countGroup . parseContent

-- * SECOND problem
day' :: Text -> Int
day' = countGarbage . parseContent

-- * Tests

examples = [
  "{}",
  "{{{}}}",
  "{{},{}}",
  "{{{},{},{{}}}}",
  "{<{},{},{{}}>}",
  "{<a>,<a>,<a>,<a>}",
  "{{<a>},{<a>},{<a>},{<a>}}",
  "{{<!>},{<!>},{<!>},{<a>}}"
  ]

test :: IO ()
test = hspec $ do
  describe "simple examples" $ do
    it "of first star" $ do
      map day examples `shouldBe` [1,6,5,16,1,1,9,3]
    it "of second star" $ do
      map day' examples `shouldBe` [0,0,0,0,10,4,4,13]
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 14421
    it "on second star" $ do
      day' fileContent `shouldBe` 6817

-- start: 11:44
-- first star: 12:04
-- second star: 12:05
