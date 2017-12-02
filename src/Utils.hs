module Utils (
  module Utils
  , module Test.Hspec
  , module Debug.Trace
  , HashMap
  , Set
  , Map
  , Vector
  , module Data.Function.Memoize
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , readMaybe
  , module Data.Function
  , module Data.Functor
  , module Control.Monad
  , module Data.Foldable
  , module Data.Traversable
  , (<>)
  , module Data.Char
             ) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)

import Control.Parallel.Strategies (parBuffer, using, rdeepseq)

import Data.List.Split (chunksOf)

import qualified Data.Set as Set
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Vector (Vector)

import qualified Data.ByteString.Char8 as BS

import Data.FileEmbed (embedStringFile)

import Data.Char
import Data.Monoid ((<>))

import Test.Hspec
import qualified Data.Vector as V

import Data.Char (toLower)

import Language.Haskell.TH.Syntax
import Data.Void
import Control.Monad (void)

import Text.Read (readMaybe)
import Debug.Trace

import Data.Function.Memoize
import Data.Function
import Data.Functor
import Data.Foldable
import Data.Traversable

-- So I can use it in the shell
-- dayX <$$> content

(<$$>) f x = (fmap . fmap) f x

infixl 4 <$$>

-- * Torus enum

-- |
-- >>> data Test = A | B | C | D deriving (Bounded, Enum, Show)
-- >>> succWrap A
-- B
-- >>> succWrap D
-- A
-- >>> predWrap D
-- C
-- >>> predWrap A
-- D
succWrap :: forall t. (Enum t, Bounded t) => t -> t
succWrap = nWrap 1

predWrap :: forall t. (Enum t, Bounded t) => t -> t
predWrap = nWrap (-1)

nWrap :: forall t. (Enum t, Bounded t) => Int -> t -> t
nWrap d e = let idx = fromEnum e
                m = (fromEnum (maxBound :: t)) + 1
            in toEnum ((idx + d) `mod` m)

countItem x l = countIf (==x) l

countIf p l = length (filter p l)


bfs :: Ord p => (Set p -> Set p -> Int -> Bool) -> p -> (p -> [p]) -> (Set p, Set p, Int)
bfs stopCriterion start stepFunction = go (Set.singleton start) (Set.empty) 0
  where go todos visited depth
          | stopCriterion todos visited depth = (todos, visited, depth)
          | otherwise = let newSteps = Set.fromList (mconcat (map stepFunction (Set.toList todos)))
                            okSteps = Set.difference newSteps visited

                        in go okSteps (Set.union todos visited) (depth + 1)

md5 = encode . hash
md5s = BS.unpack . encode . hash . BS.pack

parBufferChunks l = let chunks = (chunksOf 4096 l)
                    in mconcat chunks `using` parBuffer 20 rdeepseq

--

(!?) :: [a] -> Int -> Maybe a
l !? idx
  | idx < length l = Just (l !! idx)
  | otherwise = Nothing

getFile :: Q Exp
getFile = fmap loc_module qLocation >>= \name -> embedStringFile ("content/" <> map toLower name)

zipIndex :: V.Vector t -> V.Vector (Int, t)
zipIndex v = V.zip (V.enumFromN 0 (V.length v)) v

-- * Parsing

type Parser t = Parsec Void String t

sc :: Parser ()
sc = L.space (() <$ spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol_ :: String -> Parser ()
symbol_ s = void (symbol s)

-- | Wrapper around parse, to avoid the Right unpacking which is painful
-- in a competitive context
parse' :: Parser t -> String -> t
parse' parser s = case parse parser "" s of
  Right r -> r
  Left err -> error (show err)

select :: [t] -> [(t, [t])]
select [] = []
select (x:xs) = (x, xs):((x:) <$$> (select xs))
