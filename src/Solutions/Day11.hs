{-# LANGUAGE TypeFamilies #-}

module Solutions.Day11 (day11, parser, part1, part2, sumOfGalaxyDistances) where

import qualified Data.Sequence as S
import Lib.AOC (runSolution)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat

type Input = Matrix Char

parser :: String -> Input
parser = S.fromList . map S.fromList . lines

getExpandedGalaxyLocations :: Int -> Input -> [(Int, Int)]
getExpandedGalaxyLocations expansionFactor universe = 
  [ expandedLoc | 
    (locX, locY) <- Mat.findLocations (== '#') universe,
        let 
          xIncrease = (expansionFactor - 1) * length (filter (< locX) emptyCols)
          yIncrease = (expansionFactor - 1) * length (filter (< locY) emptyRows)
          expandedLoc = (locX + xIncrease, locY + yIncrease)
      ]
  where
    emptyRows = Mat.findRowIndices (all (== '.')) universe
    emptyCols = Mat.findColIndices (all (== '.')) universe

sumOfGalaxyDistances :: Int -> Input -> Int
sumOfGalaxyDistances expansionFactor universe = distances galaxyLocations
 where
  galaxyLocations = getExpandedGalaxyLocations expansionFactor universe

  -- Straight line distance between two points
  findDistance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

  distances [] = 0
  distances (x : xs) = sum (map (findDistance x) xs) + distances xs


part1 :: Input -> Int
part1 = sumOfGalaxyDistances 2

part2 :: Input -> Int
part2 = sumOfGalaxyDistances 1_000_000

day11 :: IO ()
day11 = runSolution "11" parser part1 part2
