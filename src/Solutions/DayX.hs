module Solutions.DayX (dayX, parser, part1, part2) where

import Lib.AOC (runSolution)

type Input = String

parser :: String -> Input
parser = id

part1 :: Input -> String
part1 = id

part2 :: Input -> String
part2 = id

dayX :: IO ()
dayX = runSolution "X" parser part1 part2
