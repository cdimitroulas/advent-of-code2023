module Solutions.Day06 (day06, parser, part1, part2) where

import Lib.AOC (runSolution)

type Input = [(Int, Int)]

parser :: String -> Input
parser = (\[l1,l2] -> zip l1 l2) . map (map read . drop 1 . words) . lines

distances :: Int -> [Int]
distances time = map (\x -> x * (time - x)) [0 .. time]

numOfWinningOptions :: (Int, Int) -> Int
numOfWinningOptions (time, recordDistance) = length $ filter (> recordDistance) (distances time)

part1 :: Input -> Int
part1 = product . map numOfWinningOptions

part2 :: Input -> Int
part2 input = numOfWinningOptions fixedInput 
  where
    fixedInput = (combineNumbers $ map fst input, combineNumbers $ map snd input)
    combineNumbers = read . concatMap show

day06 :: IO ()
day06 = runSolution "06" parser part1 part2
