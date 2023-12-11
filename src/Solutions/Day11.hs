{-# LANGUAGE TypeFamilies #-}

module Solutions.Day11 (day11, parser, part1, part2) where

import Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Sequence as S
import Lib.AOC (runSolution)
import Lib.SearchAlgorithms (DijkstraGraph(..), Distance(..), findShortestDistance)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat
import Data.Foldable (foldl', toList)
import Debug.Trace
import Data.Sequence (Seq((:<|)))
import Data.Maybe (catMaybes)
import Lib.List (pairs)

type Input = Matrix Char

parser :: String -> Input
parser =  S.fromList . map S.fromList . lines

isSpace :: Char -> Bool
isSpace = (==) '.'

-- Modifes the indexes to insert at to account for the fact that insert one index will change the position where
-- the next insertion needs to occur.
indicesToInsertAt :: [Int] -> [Int]
indicesToInsertAt [] = []
indicesToInsertAt (x:xs) = x : indicesToInsertAt (map (+ 1) xs)

expandUniverse :: Input -> Input
expandUniverse universe =
  -- We need to use indicesToInsertAt because each time we add a row the indexes where we need to insert new rows will move down
  -- by 1. Same logic applies to inserting columns.
  let emptyRowIndices =  indicesToInsertAt $ Mat.findRowIndices (all isSpace) universe
      emptyRow = S.fromList $ replicate (Mat.width universe) '.'
      universeWithRowsExpanded = foldl' (flip $ Mat.insertRow emptyRow) universe emptyRowIndices
      emptyColIndices = indicesToInsertAt $ Mat.findColIndices (all isSpace) universeWithRowsExpanded
      emptyCol = S.fromList $ replicate (Mat.height universeWithRowsExpanded) '.'
  in foldl' (flip $ Mat.insertCol emptyCol) universeWithRowsExpanded emptyColIndices

-- Just for debugging the state of the "universe" to see what it looks like after expansion
prettyPrint :: Input -> String
prettyPrint S.Empty = ""
prettyPrint (x :<| xs) = show (toList x) ++ "\n" ++ prettyPrint xs

part1 :: Input -> Int
part1 input = distances galaxyLocations
  where
    expandedUniverse = expandUniverse input
    galaxyLocations = Mat.findLocations (== '#') expandedUniverse
    findDistance (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

    distances [] = 0
    distances (x:xs) = sum (map (findDistance x) xs) + distances xs

newtype Graph2D = Graph2D (Array (Int, Int) Char) deriving (Show)

-- matrixToGraph2D :: Input -> Graph2D
-- matrixToGraph2D mat = Graph2D $ A.listArray ((0, 0), (Mat.height mat - 1, Mat.width mat - 1)) $ concat $ Mat.toList mat

-- instance DijkstraGraph Graph2D where
--   type DijkstraNode Graph2D = (Int, Int)
--   type DijkstraCost Graph2D = Int
--   -- the cost is always 1 for our graph
--   dijkstraEdges (Graph2D arr) node = [(neighbor, 1) | neighbor <- getNeighbors node arr]

-- getNeighbors :: (Int, Int) -> Array (Int, Int) Char -> [(Int, Int)]
-- getNeighbors (x, y) arr = catMaybes [up, down, left, right]
--   where
--     (maxRow, maxCol) = snd $ A.bounds arr
--     up = if y - 1 < 0 then Nothing else Just (x, y - 1)
--     down = if y + 1 > maxRow then Nothing else Just (x, y + 1)
--     left = if x - 1 < 0 then Nothing else Just (x - 1, y)
--     right = if x + 1 > maxCol then Nothing else Just (x + 1, y)

part2 :: Input -> String
part2 = show . const 0

day11 :: IO ()
day11 = runSolution "11" parser part1 part2
