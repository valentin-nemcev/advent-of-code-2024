module AdventOfCode2024 where

import Data.HashMap.Lazy ()
import Data.IntMap.Strict qualified as IntMap
import Data.Ix (Ix (inRange))
import Data.String.Here (here, i)
import Data.Tuple.Extra (both, dupe)
import Relude.Extra.Map (lookupDefault)
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

input :: Int -> IO String
input n = readFileBS [i|./input/${n}.txt|] <&> decodeUtf8

-----

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = zipWith f <*> drop 1

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

-----

day1_example :: String
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
-- ([3,4,2,1,3,3],[4,3,5,3,9,3])
day1P :: ReadP ([Int], [Int])
day1P = unzip <$> (linesP $ do a <- readDecP; spaceP; b <- readDecP; return (a, b))

day1a :: String -> Int
day1a = parse day1P >>> both sort >>> (uncurry $ zipWith \a b -> abs (a - b)) >>> sum

day1b :: String -> Int
day1b = parse day1P >>> (second $ makeLookup) >>> (\(as, lookup) -> lookup <$> as) >>> sum
  where
    makeLookup :: [Int] -> Int -> Int
    makeLookup as =
      let m = IntMap.fromListWith (+) $ map dupe as
       in \k -> lookupDefault 0 k m

-- >>> (day1a &&& day1b) day1_example
-- >>> (day1a &&& day1b) <$> input 1
-- (11,31)
-- (2815556,23927637)

day2_example :: String
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
day2 = parse day2P >>> (map one &&& map ((:) <*> dampenedLevels)) >>> (both $ count $ any $ isSafe)

isSafe :: [Int] -> Bool
isSafe ds =
  let diff = mapAdjacent (-) ds
   in or (all <$> inRange <$> [(-3, -1), (1, 3)] ?? diff)

-- >>> dampenedLevels [1, 2, 3, 4, 5]
-- [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
dampenedLevels :: [Int] -> [[Int]]
dampenedLevels = zipWith (++) <$> inits <*> drop 1 . tails

-- >>> day2 day2_example
-- >>> day2 <$> input 2
-- (2,4)
-- (490,536)
