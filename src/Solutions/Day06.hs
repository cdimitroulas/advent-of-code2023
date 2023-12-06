module Solutions.Day06 (day06, parser, part1, part2) where

import Lib.AOC (runSolution)
import Data.Foldable (Foldable(foldl'))

type Input = [(Int, Int)]

parser :: String -> Input
parser = (\[l1,l2] -> zip l1 l2) . map (map read . drop 1 . words) . lines

generateHistogram :: Int -> [Int]
generateHistogram time = map (\x -> x * (time - x)) [0 .. time]

numOfWinningOptions :: (Int, Int) -> Int
numOfWinningOptions (time, recordDistance) = length $ filter (> recordDistance) (generateHistogram time)

part1 :: Input -> Int
part1 = product . map numOfWinningOptions

part2 :: Input -> Int
part2 input = numOfWinningOptions $ fixInput input
  where
    combineNumbers n1 n2 = read (show n1 ++ show n2)

    fixInput = foldl' (\(totalT, totalD) (t, d) -> (combineNumbers totalT t, combineNumbers totalD d)) (0, 0)

day06 :: IO ()
day06 = runSolution "06" parser part1 part2
