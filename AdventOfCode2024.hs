module AdventOfCode2024 where

import Control.Arrow (Arrow (second), (&&&), (***), (>>>))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.IntMap.Strict qualified as M
import Data.Ix (Ix (inRange))
import Data.List (inits, singleton, sort, tails)
import Data.List.Extra (allSame)
import Data.String.Here (here)
import Data.Tuple.Extra (both)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readDecP)

parse :: (Show a) => ReadP a -> String -> a
parse p =
  readP_to_S (p <* P.eof) >>> \case
    [(r, "")] -> r
    _ -> error "parse error"

spaceP = P.skipMany1 (P.char ' ')

linesP :: ReadP a -> ReadP [a]
linesP p = p `P.sepBy` nl <* P.optional nl
  where
    nl = P.char '\n'

-----

dup :: a -> (a, a)
dup x = (x, x)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

infixl 4 ??

(??) :: (Functor f) => f (a -> b) -> a -> f b
(??) ff x = (\f -> f x) <$> ff

flap :: (Functor f) => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff

-----

data Part = A | B deriving (Enum)

-----

day1_example =
  [here|
3   4
4   3
2   5
1   3
3   9
3   3
|]

-- >>> parse day1P day1_example
-- [(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]
day1P :: ReadP ([Int], [Int])
day1P = unzip <$> (linesP $ do a <- readDecP; P.skipSpaces; b <- readDecP; return (a, b))

day1a :: String -> Int
day1a = parse day1P >>> both sort >>> (uncurry $ zipWith \a b -> abs (a - b)) >>> sum

day1b :: String -> Int
day1b = parse day1P >>> (second $ makeLookup) >>> (\(as, lookup) -> lookup <$> as) >>> sum
  where
    makeLookup :: [Int] -> Int -> Int
    makeLookup as =
      let m = M.fromListWith (+) $ map dup as
       in \k -> M.findWithDefault 0 k m

-- >>> (day1a &&& day1b) day1_example
-- >>> (day1a &&& day1b) <$> readFile "./input/1.txt"
-- (11,31)
-- (2815556,23927637)

day2_example =
  [here|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|]

-- >>> parse day2P day2_example
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
day2P :: ReadP [[Int]]
day2P = linesP $ readDecP `P.sepBy1` spaceP

day2 :: String -> (Int, Int)
day2 = parse day2P >>> (map singleton &&& map ((:) <*> dampenedLevels)) >>> (both $ count $ any $ isSafe)

isSafe :: [Int] -> Bool
isSafe ds =
  let diff = mapAdjacent (-) ds
   in or (all <$> inRange <$> [(-3, -1), (1, 3)] ?? diff)

-- >>> dampenedLevels [1, 2, 3, 4, 5]
-- [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
dampenedLevels :: [Int] -> [[Int]]
dampenedLevels = zipWith (++) <$> inits <*> drop 1 . tails

-- >>> day2 day2_example
-- >>> day2 <$> readFile "./input/2.txt"
-- (2,4)
-- (490,536)
