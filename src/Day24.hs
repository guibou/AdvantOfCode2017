module Day24 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as Map
import qualified Data.Set as Set

-- start 11h47
-- first start 12h10
-- second start 12h12

data Pin = Pin Int
  deriving (Show, Ord, Eq)

data Component = Component Pin Pin
  deriving (Show, Ord, Eq)



fileContent :: [Component]
fileContent = parseContent $(getFile)

parseItem :: Parser Component
parseItem = Component <$> (Pin <$> L.decimal) <*> (char '/' *> (Pin <$> L.decimal))

parseContent :: Text -> [Component]
parseContent t = unsafeParse (parseItem `sepBy` char '\n') t

-- | Returns a map associating a port number to a port with this number as first or second position
-- So port are duplicated (and flipped) in the map
toMap :: [Component] -> Map Pin (Set Component)
toMap ports = Map.fromListWith (Set.union) (concatMap (\p@(Component a b) -> [(a, Set.singleton p), (b, Set.singleton (flipComponent p))]) ports)

-- * Generics
process :: [Component] -> [(Int, Int)]
process ports = go (Pin 0) 0 0 (toMap ports)
  where
    go :: Pin -> Int -> Int -> Map Pin (Set Component) -> [(Int, Int)]
    go inPort !currentScore !currentLength availableComponents =
      let okComponents = fromMaybe (Set.empty) (Map.lookup inPort availableComponents)
      in if null okComponents
         then [(currentLength, currentScore)]
         else do
        -- inPort == _inPort'
        aComponent@(Component _inPort' outPort) <- Set.toList okComponents

        let
          -- delete the port in both configurations
          availableComponents' = Map.adjust (Set.delete aComponent) inPort availableComponents
          availableComponents'' = Map.adjust (Set.delete (flipComponent aComponent)) outPort availableComponents'

          newScore = currentScore + portScore aComponent
        go outPort newScore (currentLength + 1) availableComponents''

flipComponent :: Component -> Component
flipComponent (Component a b) = Component b a

portScore :: Component -> Int
portScore (Component (Pin a) (Pin b)) = a + b

-- * FIRST problem
day :: [Component] -> Int
day ports = maximum $ map snd (process ports)

-- * SECOND problem
day' :: [Component] -> Int
day' ports = snd $ maximum $ process ports

-- * Tests

testData :: [Component]
testData = parseContent [here|
0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testData `shouldBe` 31
    it "of second star" $ do
      day' testData `shouldBe` 19
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1859
    it "on second star" $ do
      day' fileContent `shouldBe` 1799
