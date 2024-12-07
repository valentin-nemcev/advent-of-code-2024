{- cabal:
build-depends:
  base ^>=4.20.0.0,
  here ^>=1.2.14
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module AdventOfCode2024 where

import Control.Arrow (Arrow (second), (***), (>>>), (|||))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.List (sort)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readDecP)

parse :: (Show a) => ReadP a -> String -> a
parse p =
  readP_to_S (p <* P.eof) >>> \case
    [] -> error "nothing parsed"
    [(r, "")] -> r

-- case  input of
--   ([(result, "")]) -> result
--   ([(result, leftover)]) -> error $ "unparsed " ++ show leftover ++ ", parsed " ++ show result
--   [] -> error "nothing parsed"
--   r -> error $ show r

linesP :: ReadP a -> ReadP [a]
linesP p = p `P.endBy1` P.char '\n'

dup :: a -> (a, a)
dup x = (x, x)

day1a_example =
  unlines
    [ "3   4",
      "4   3",
      "2   5 ",
      "1   3",
      "3   9",
      "3   3"
    ]

-- >>> day1a_example
-- >>> day1a_example2
-- "3   4\n4   3\n2   5 \n1   3\n3   9\n3   3\n"

day1a_parser :: ReadP [(Int, Int)]
day1a_parser = linesP $ do a <- readDecP; P.skipSpaces; b <- readDecP; return (a, b)

-- >>> parse day1a_parser day1a_example

-- *** Exception: /Users/valentin/Personal/advent-of-code-2024/AdventOfCode2024.hs:15:39-58: Non-exhaustive patterns in \case

day1a = parse day1a_parser >>> unzip >>> (sort *** sort) >>> (uncurry $ zipWith \a b -> abs (a - b)) >>> sum

-- day1b :: String -> Int
day1b = parse day1a_parser >>> unzip >>> (second $ makeLookup) >>> (\(as, lookup) -> lookup `map` as) >>> sum
  where
    makeLookup :: [Int] -> Int -> Int
    makeLookup as =
      let m = M.fromListWith (+) $ map dup as
       in \k -> M.findWithDefault 0 k m

-- >>> day1a day1a_example
-- 11

-- >>> day1b day1a_example
-- [9,4,0,0,9,9]

-- >>> day1a <$> readFile "./input/1.txt"
-- >>> day1b <$> readFile "./input/1.txt"
-- 2815556
-- 23927637
