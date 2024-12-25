module AdventOfCode2024 where

import Data.Char (isUpper)
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
    [] -> error "no parse"
    _ -> error "ambiguous parse"

intP :: ReadP Int
intP = readDecP @Int

spaceP :: ReadP ()
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

mapWithIndex :: (Num a, Enum a) => (a -> b -> c) -> [b] -> [c]
mapWithIndex f = zipWith f [0 ..]

-----

day1Example :: String
day1Example =
  [here|
3   4
4   3
2   5
1   3
3   9
3   3
|]

-- >>> parse day1P day1Example
-- ([3,4,2,1,3,3],[4,3,5,3,9,3])
day1P :: ReadP ([Int], [Int])
day1P = unzip <$> (linesP $ do a <- intP; spaceP; b <- intP; return (a, b))

day1a :: String -> Int
day1a = parse day1P >>> both sort >>> (uncurry $ zipWith \a b -> abs (a - b)) >>> sum

day1b :: String -> Int
day1b = parse day1P >>> (second $ makeLookup) >>> (\(as, lookup) -> lookup <$> as) >>> sum
  where
    makeLookup :: [Int] -> Int -> Int
    makeLookup as =
      let m = IntMap.fromListWith (+) $ map dupe as
       in \k -> lookupDefault 0 k m

-- >>> (day1a &&& day1b) day1Example
-- >>> (day1a &&& day1b) <$> input 1
-- (11,31)
-- (2815556,23927637)

-----

day2Example :: String
day2Example =
  [here|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|]

-- >>> parse day2P day2Example
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
day2P :: ReadP [[Int]]
day2P = linesP $ intP `P.sepBy1` spaceP

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

-- >>> day2 day2Example
-- >>> day2 <$> input 2
-- (2,4)
-- (490,536)

-----

day3Example :: String
day3Example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

data Instruction = Do | Don't | Mul (Int, Int) deriving (Show, Eq)

--- >>> readP_to_S (day3P <* P.eof) day3Example
-- [([Mul (2,4),Don't,Mul (5,5),Mul (11,8),Do,Mul (8,5)],"")]
day3P :: ReadP [Instruction]
day3P = catMaybes <$> (P.many $ (Just <$> instruction) P.<++ (Nothing <$ P.get))
  where
    instruction = (Do <$ P.string "do()") P.+++ (Don't <$ P.string "don't()") P.+++ (Mul <$> mul)
    mul = (,) <$> (P.string "mul(" *> intP) <*> (P.char ',' *> intP <* P.char ')')

day3 :: String -> (Int, Int)
day3 = parse day3P >>> map interpretA &&& snd . mapAccumL interpretB True >>> both sum
  where
    interpretA :: Instruction -> Int
    interpretA = \case Mul (a, b) -> a * b; _ -> 0

    interpretB :: Bool -> Instruction -> (Bool, Int)
    interpretB enabled (Mul (a, b)) = (enabled, if enabled then a * b else 0)
    interpretB _ Don't = (False, 0)
    interpretB _ Do = (True, 0)

--- >>> day3 day3Example
--- >>> day3 <$> input 3
-- (161,48)
-- (175615763,74361272)

-----
day4Example :: String
day4Example =
  [here|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|]

-- >>> parse day4P day4Example
-- ["MMMSXXMASM","MSAMXMSMSA","AMXSXMAAMM","MSAMASMSMX","XMASAMXAMM","XXAMMXXAMA","SMSMSASXSS","SAXAMASAAA","MAMMMXMMMM","MXMXAXMASX"]
day4P :: ReadP [[Char]]
day4P = linesP $ P.munch $ isUpper

day4a = parse day4P >>> flap [id, map reverse, transpose, mapWithIndex rotate >>> transpose, mapWithIndex (rotate . negate) >>> transpose] >>> concat . concat >>> sublists 4 >>> count (== "XMAS")

diag = mapWithIndex rotate >>> transpose

rotate n xs =
  let xs' = ":" ++ xs ++ ":"
      nn = n `mod` length xs'
   in drop nn xs' ++ take nn xs'

sublists n = tails >>> (map $ take n)

--- >>> (parse day4P >>> mapWithIndex (rotate . negate)  ) $ day4Example
-- [":MMMSXXMASM:","::MSAMXMSMSA","M::AMXSXMAAM","MX::MSAMASMS","AMM::XMASAMX","XAMA::XXAMMX","ASXSS::SMSMS","MASAAA::SAXA","MMXMMMM::MAM","MXAXMASX::MX"]
--
--

t =
  [ "MMMSXXMASM:",
    "SAMXMSMSA:M",
    "XSXMAAMM:AM",
    "MASMSMX:MSA",
    "AMXAMM:XMAS",
    "XXAMA:XXAMM",
    "SXSS:SMSMSA",
    "AAA:SAXAMAS",
    "MM:MAMMMXMM",
    "X:MXMXAXMAS"
  ]

tt =
  [ "M:MMMSXXMAS",
    "SA:MSAMXMSM",
    "AMM:AMXSXMA",
    "MSMX:MSAMAS",
    "MXAMM:XMASA",
    "MXXAMA:XXAM",
    "MSASXSS:SMS",
    "XAMASAAA:SA",
    "AMMMXMMMM:M",
    "MXMXAXMASX:"
  ]

--- >>> day4a day4Example
-- 10

--- >>> mod (-1) 5
-- 4

-- >>> :t rotate . negate
-- rotate . negate :: Int -> [Char] -> [Char]
