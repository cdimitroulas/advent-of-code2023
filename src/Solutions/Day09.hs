module Solutions.Day09 (day09, parser, part1, part2) where

import Lib.AOC (runSolution)
import Data.List (foldl')

type Input = [[Int]]

data ExtrapolateDir = Forward | Back deriving (Show, Eq)

parser :: String -> Input
parser = map (map read . words). lines

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (x1:x2:xs) = x2 - x1 : differences (x2:xs)

extrapolateSequence :: ExtrapolateDir -> [Int] -> [Int]
extrapolateSequence dir xs = if dir == Forward then xs <> [go [] xs] else go [] xs : xs
    where
      combiner = if dir == Forward then (+) else (-)
      accessor = if dir == Forward then last else head
      foldFn = foldl' (flip combiner) 0

      go :: [[Int]] -> [Int] -> Int
      go diffs next
        | all (== 0) next = accessor xs `combiner` foldFn (map accessor diffs)
        | otherwise = let seqDifferences = differences next
                      in go (seqDifferences : diffs) seqDifferences

part1 :: Input -> Int
part1 = sum . map (last . extrapolateSequence Forward)

part2 :: Input -> Int
part2 = sum . map (head . extrapolateSequence Back)

day09 :: IO ()
day09 = runSolution "09" parser part1 part2
