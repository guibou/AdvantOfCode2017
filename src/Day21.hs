{-# Language FlexibleInstances #-}
module Day21 where

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Hashable

import qualified Data.Matrix as M
import qualified Data.HashMap.Strict as HashMap

-- 9h54
-- 11h19: star 1
-- goto fly..`

type Rule = (M.Matrix Pixel, M.Matrix Pixel)

fileContent :: [Rule]
fileContent = parseContent $(getFile)

data Pixel = On | Off deriving (Show, Eq, Generic, Hashable)

instance Hashable (M.Matrix Pixel) where
  hashWithSalt s m = foldl' hashWithSalt s m

parsePixel :: Parser Pixel
parsePixel = On <$ (char '#') <|> Off <$ (char '.')

parsePattern :: Parser (M.Matrix Pixel)
parsePattern = M.fromLists <$> some parsePixel `sepBy` char '/'

parseExpr :: Parser Rule
parseExpr = (,) <$> parsePattern <*> (string " => " *> parsePattern)

parseContent :: Text -> [Rule]
parseContent t = unsafeParse (parseExpr `sepBy` char '\n') t

fmtMatrix m = show (fmap f m)
  where
    f On = '#'
    f Off = '.'

displayAllRules :: [Rule] -> IO ()
displayAllRules rules = mapM_ f rules
  where
    f (fromRule, toRule) = do
      putText "========"
      putText (fmtMatrix fromRule)
      putText "==>"
      putText (fmtMatrix toRule)

-- * Generics
startPattern = M.fromLists [
  [Off, On, Off],
  [Off, Off, On],
  [On, On, On]]

extendRule :: M.Matrix Pixel -> [M.Matrix Pixel]
extendRule m = mconcat (map (\m' -> map (\rot -> applyN rot rotateM m') [0..3]) [m, mirrorX m, mirrorY m])

extendBook :: [Rule] -> [Rule]
extendBook = concatMap (\(fromRule, toRule) -> (,toRule) <$> extendRule fromRule)

rotateM m = M.matrix r c f
  where
    r = M.nrows m
    c = M.ncols m
    f (cr, cc) = M.unsafeGet cc ((r - cr + 1)) m

mirrorX m = M.matrix r c f
  where
    r = M.nrows m
    c = M.ncols m
    f (cr, cc) = M.unsafeGet cr (c - cc + 1) m

mirrorY m = M.matrix r c f
  where
    r = M.nrows m
    c = M.ncols m
    f (cr, cc) = M.unsafeGet (r - cr + 1) cc m

splitMatrix :: Int -> M.Matrix t -> M.Matrix (M.Matrix t)
splitMatrix n m = M.matrix s s f
  where
    s = M.ncols m `div` n
    f (l', c') = let (l, c) = (l' - 1, c' - 1) in M.submatrix (l * n + 1) ((l + 1) * n) (c * n + 1) ((c + 1) * n) m

joinMatrix :: M.Matrix (M.Matrix t) -> M.Matrix t
joinMatrix m' = let m = M.toLists m'
                in unsafeFromJust $ foldr1May (M.<->) (map (\ml -> unsafeFromJust $ foldr1May (M.<|>) ml) m)

-- * FIRST problem
process :: [Rule] -> Int -> M.Matrix Pixel
process book n = applyN n go startPattern
  where
    tr = transform (HashMap.fromList $ extendBook book)
    go :: M.Matrix Pixel -> M.Matrix Pixel
    go pat = joinMatrix (map tr (splitMatrix (if M.ncols pat `mod` 2 == 0 then 2 else 3) pat))

transform :: HashMap (M.Matrix Pixel) (M.Matrix Pixel) -> M.Matrix Pixel -> M.Matrix Pixel
transform rules m = unsafeFromJust (HashMap.lookup m rules)

job :: [Rule] -> Int -> Int
job book n = countIf (==On) (M.toList (process book n))

day book = job book 5

-- * SECOND problem
day' book = job book 18

-- * Tests
testBook = parseContent [here|../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#|]


test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      job testBook 2  `shouldBe` 12
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 150
    it "on second star" $ do
      day' fileContent `shouldBe` undefined
