module Solutions.Day09 (day09, parser, part1, part2) where

import Lib.AOC (runSolution)

type Input = [[Int]]

data ExtrapolateDir = Forward | Back deriving (Show, Eq)

parser :: String -> Input
parser = map (map read . words). lines

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (x1:x2:xs) = x2 - x1 : differences (x2:xs)

extrapolateSequence1 :: [Int] -> Int
extrapolateSequence1 xs = go [] xs
    where
      go :: [Int] -> [Int] -> Int
      go diffs next
        | all (== 0) next = last xs + sum diffs
        | otherwise = let seqDifferences = differences next
                      in go (last seqDifferences : diffs) seqDifferences

part1 :: Input -> Int
part1 = sum . map extrapolateSequence1

-- Extrapolating backwards is just the same as extrapolating forwards on the reversed list 
part2 :: Input -> Int
part2 = part1 . map reverse

day09 :: IO ()
day09 = runSolution "09" parser part1 part2
