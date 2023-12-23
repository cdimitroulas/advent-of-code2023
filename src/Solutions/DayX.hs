module Solutions.DayX (dayX, parser, part1, part2) where

import Lib.AOC (runSolution)

type Input = String

parser :: String -> Input
parser = id

part1 :: Input -> Int
part1 = const 0

part2 :: Input -> Int
part2 = const 0

dayX :: IO ()
dayX = runSolution "X" parser part1 part2
