module Day22 where

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec

import Data.Hashable

import qualified Data.HashMap.Strict as HashMap
-- 16h54
-- 17h36 (first star, pause for children)
-- back to second star 22h37
-- second star: 23h16

fileContent :: Game Status
fileContent = parseContent $(getFile)

data Status = Clean | Infected
  deriving (Show, Eq, Enum, Bounded)

data ExtendedStatus = CleanE | WeakenedE | InfectedE | FlaggedE
  deriving (Show, Eq, Enum, Bounded)

parseNode :: Parser Status
parseNode = (Clean <$ char '.') <|> (Infected <$ char '#')

parseGrid :: Parser [[Status]]
parseGrid = many parseNode `sepBy` char '\n'

toSet :: [[Status]] -> HashMap Position Status
toSet status = HashMap.fromList $ do
  (y, line) <- zip [0..] status
  (x, val) <- zip [0..] line

  pure (Position x y, val)

parseContent t = Game (toSet grid) (initCarrier $ Position (rows `div` 2) (cols `div` 2))
  where
    grid = unsafeParse parseGrid t
    rows = length grid
    cols = length (unsafeHead grid)

-- * Generics
data Direction = UpD | RightD | DownD | LeftD
  deriving (Show, Enum, Bounded, Eq)

data Carrier = Carrier Position Direction
  deriving (Show)

data Position = Position Int Int
  deriving (Show, Generic, Hashable, Eq)

data Game status = Game (HashMap Position status) Carrier
  deriving (Show)

turnRightCarrier (Carrier p d) = (Carrier p (cycleSucc d))
turnLeftCarrier (Carrier p d) = (Carrier p (cyclePred d))

initCarrier :: Position -> Carrier
initCarrier p = Carrier p UpD

currentGameStatus :: Bounded status => Game status -> status
currentGameStatus (Game grid (Carrier position _)) = fromMaybe minBound (HashMap.lookup position grid)

changeStep :: (Eq status, Enum status, Bounded status) => HashMap Position status -> Position -> HashMap Position status
changeStep s p =
  let
    currentStatus = fromMaybe minBound (HashMap.lookup p s)
  in HashMap.insert p (cycleSucc currentStatus) s

turn :: (Bounded status, Eq status, Enum status) => status -> (status -> Carrier -> Carrier) -> Game status -> (Bool, Game status)
turn infected turnFunction g@(Game grid carrier@(Carrier p _)) =
  let
    currentStatus = currentGameStatus g

    carrier' = turnFunction currentStatus carrier
    grid' = changeStep grid p
  in (currentStatus == cyclePred infected, Game grid' (goForward carrier'))

goForward :: Carrier -> Carrier
goForward (Carrier (Position x y) d) = Carrier (Position x' y') d
  where (x', y') = case d of
          UpD -> (x, y - 1)
          DownD -> (x, y + 1)
          LeftD -> (x - 1, y)
          RightD -> (x + 1, y)

getX (Position x _) = x
getY (Position _ y) = y

displayGrid :: (Bounded status) => (status -> Text) -> Game status -> IO ()
displayGrid statusToTextF (Game grid _) =
  let
    items = HashMap.keys grid
    minX = minimum (map getX items)
    minY = minimum (map getY items)
    maxX = maximum (map getX items)
    maxY = maximum (map getY items)

  in
    for_ [minY .. maxY] $ \y -> do
      for_  [minX .. maxX] $ \x -> do
        putStr $ statusToTextF (fromMaybe minBound $ HashMap.lookup (Position x y) grid)
      putText $ ""

doit :: (Bounded status, Eq status, Enum status) => status -> (status -> Carrier -> Carrier) -> Int -> Game status -> (Game status, Int)
doit infectedF turnStepF nMax game = go game 0 0
  where go g !c n
          | n == nMax = (g, c)
          | otherwise = let (infected, g') = turn infectedF turnStepF g
                        in go g' (if infected then c + 1 else c) (n + 1)

-- * FIRST problem
turnStep :: Status -> Carrier -> Carrier
turnStep Infected = turnRightCarrier
turnStep Clean = turnLeftCarrier

day :: Game Status -> Int
day game = snd $ doit Infected turnStep 10000 game

statusToText Clean = "."
statusToText Infected = "#"

-- * SECOND problem
turnStep' :: ExtendedStatus -> Carrier -> Carrier
turnStep' CleanE = turnLeftCarrier
turnStep' WeakenedE = identity
turnStep' InfectedE = turnRightCarrier
turnStep' FlaggedE = turnLeftCarrier . turnLeftCarrier -- face backward

toExtendedStatus Clean = CleanE
toExtendedStatus Infected = InfectedE

toExtended :: Game Status -> Game ExtendedStatus
toExtended (Game grid carrier) = Game (map toExtendedStatus grid) carrier

day' :: Game Status -> Int
day' game = snd $ doit InfectedE turnStep' 10000000 (toExtended game)

statusToTextE CleanE = "."
statusToTextE InfectedE = "#"
statusToTextE WeakenedE = "W"
statusToTextE FlaggedE = "F"

-- 2488118: too low

-- * Tests
testContent :: Game Status
testContent = parseContent [here|
..#
#..
...|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testContent `shouldBe` 5587
    it "of second star" $ do
      day' testContent `shouldBe` 2511944
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 5460
    it "on second star" $ do
      day' fileContent `shouldBe` 2511702
