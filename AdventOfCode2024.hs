module AdventOfCode2024 where

import Control.Arrow (Arrow (second), (&&&), (***), (>>>))
import Data.IntMap.Strict qualified as M
import Data.List (sort)
import Data.String.Here (here)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readDecP)

parse :: (Show a) => ReadP a -> String -> a
parse p =
  readP_to_S (p <* P.eof) >>> \case
    [(r, "")] -> r
    _ -> error "parse"

linesP :: ReadP a -> ReadP [a]
linesP p = p `P.sepBy` nl <* P.optional nl
 where
  nl = P.char '\n'

dup :: a -> (a, a)
dup x = (x, x)

day1a_example =
  [here|
3   4
4   3
2   5
1   3
3   9
3   3
|]

day1_parse :: ReadP [(Int, Int)]
day1_parse = linesP $ do a <- readDecP; P.skipSpaces; b <- readDecP; return (a, b)

-- >>> parse day1_parse day1a_example
-- [(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]

day1a :: String -> Int
day1a = parse day1_parse >>> unzip >>> (sort *** sort) >>> (uncurry $ zipWith \a b -> abs (a - b)) >>> sum

day1b :: String -> Int
day1b = parse day1_parse >>> unzip >>> (second $ makeLookup) >>> (\(as, lookup) -> lookup <$> as) >>> sum
 where
  makeLookup :: [Int] -> Int -> Int
  makeLookup as =
    let m = M.fromListWith (+) $ map dup as
     in \k -> M.findWithDefault 0 k m

-- >>> (day1a &&& day1b) day1a_example
-- >>> (day1a &&& day1b) <$> readFile "./input/1.txt"
-- (11,31)
-- (2815556,23927637)
