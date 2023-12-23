module Solutions.Day12 (day12, parser, part1, part2) where

import Lib.AOC (runSolution)
import Data.Char (isDigit, digitToInt)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Debug.Trace
import Data.List (nub, intercalate)
import Data.List.Split (splitOn)

type Record = (String, [Int])

type Input = [Record]

parser :: String -> Input
parser = map ((\[x1, x2] -> (x1, map read (splitOn "," x2))) . words) . lines

allPossibleStrings :: String -> [String]
allPossibleStrings ('?':restOfStr) = map ('.' :) (allPossibleStrings restOfStr) <> map ('#' :) (allPossibleStrings restOfStr)
allPossibleStrings (c:restOfStr) = map ([c] ++) (allPossibleStrings restOfStr)
allPossibleStrings "" = [""]

stringGroupSizeParser :: P.Parser [Int]
stringGroupSizeParser = do
  P.many' (P.char '.')
  groups <- P.many1 (P.char '#') `P.sepBy` P.many1 (P.char '.')
  pure (map length groups)

bruteForce :: Record -> Int
bruteForce (str, groupSizes) = length $ nub $ filter isValidString (allPossibleStrings str)
  where
    isValidString s = P.parseOnly stringGroupSizeParser (T.pack s) == Right groupSizes

-- numOfArrangements :: (String, [Int]) -> Int
-- numOfArrangements (str, groupSizes) = undefined
--   where
--     -- The minimum string length needed to create the separate groups is the sum of the group sizes +
--     -- enough spaces to separate the groups which is always 1 less than the number of groups (to allow 1 space
--     -- between each group)
--     minLength sizes = sum sizes + (length sizes - 1)

--     --  We should filter out all complete groups e.g. ".#." or any groups at the start/end of the string like -> "##...#." and "....###" <-
--     --  Those are "fixed" so there's no need to calculate them.
--     --  Only the arrangements for the remaining string would need to then be calculated
--     -- filteredInput = _

-- -- The most minimal possible string required to make up the groups
-- minimumStr :: (String, [Int]) -> String
-- minimumStr (_, groupSizes) = intercalate "." $ map (`replicate` '#') groupSizes

-- possibleGroups "" groups = groups
-- possibleGroups (c:cs) groups =
--   if isHashOrQ c
--   then possibleGroups (dropWhile isHashOrQ cs) (groups <> [c: takeWhile isHashOrQ cs])
--   else possibleGroups cs groups

--   where
--     isHashOrQ char = char == '#' || char == '?'

part1 :: Input -> Int
part1 = sum . map bruteForce

unfold :: Record -> Record
unfold (str, groupSizes) = (intercalate "?" $ replicate 5 str, concat $ replicate 5 groupSizes)

part2 :: Input -> Int
part2 = sum . map (bruteForce . unfold)

day12 :: IO ()
day12 = runSolution "12" parser part1 part2
