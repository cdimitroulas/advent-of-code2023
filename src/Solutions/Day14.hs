module Solutions.Day14 (day14, parser, part1, part2) where

import Data.Foldable (foldl')
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import Lib.AOC (runSolution)
import Lib.List (findCycle)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat

type Input = Matrix Char

parser :: String -> Input
parser = Mat.fromList . lines

rollRockLine :: (Ord a) => (Char -> a) -> String -> String
rollRockLine sortDir = intercalate "#" . map (sortBy (comparing sortDir)) . splitOn "#"

data Dir = N | E | S | W deriving (Eq)

rollRocks :: Dir -> Input -> Input
rollRocks N = Mat.mapCols $ rollRockLine Down
rollRocks E = Mat.mapRows $ rollRockLine id
rollRocks S = Mat.mapCols $ rollRockLine id
rollRocks W = Mat.mapRows $ rollRockLine Down

totalLoad :: Matrix Char -> Int
totalLoad mat = foldl' (\total (_, y) -> total + yPosToLoadFactor y) 0 circleRockPositions
 where
  yPosToLoadFactor y = Mat.height mat - y
  circleRockPositions = Mat.findLocations (== 'O') mat

part1 :: Input -> Int
part1 = totalLoad . rollRocks N

cycleRocks :: Input -> Input
cycleRocks = rollRocks E . rollRocks S . rollRocks W . rollRocks N

part2 :: Input -> Int
part2 input =
  let cycleResults = iterate cycleRocks input
      (start, next) = findCycle cycleResults
      idx = start + (1_000_000_000 - start) `rem` (next - start)
   in totalLoad (cycleResults !! idx)

day14 :: IO ()
day14 = runSolution "14" parser part1 part2
