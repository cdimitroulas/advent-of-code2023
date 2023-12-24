module Solutions.Day16 (day16, parser, part1, part2) where

import Control.Monad.State (State)
import qualified Control.Monad.State as S
import Data.Map (Map)
import qualified Data.Map as M
import Lib.AOC (runSolution)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat

data Direction = U | D | L | R deriving (Show, Eq)

isVerticalDir :: Direction -> Bool
isVerticalDir dir = dir == U || dir == D

isHorizontalDir :: Direction -> Bool
isHorizontalDir = not . isVerticalDir

type Input = Matrix Char

parser :: String -> Input
parser = Mat.fromList . lines

type Energized = Map (Int, Int) [Direction]
type BeamState = (Direction, (Int, Int))

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

step :: BeamState -> Char -> [BeamState]
step (dir, pos) '.' = [(dir, move pos dir)]
step (dir, pos) '|'
  | isVerticalDir dir = [(dir, move pos dir)]
  | otherwise = [(U, move pos U), (D, move pos D)]
step (dir, pos) '-'
  | isHorizontalDir dir = [(dir, move pos dir)]
  | otherwise = [(L, move pos L), (R, move pos R)]
step (U, pos) '/' = [(R, move pos R)]
step (L, pos) '/' = [(D, move pos D)]
step (D, pos) '/' = [(L, move pos L)]
step (R, pos) '/' = [(U, move pos U)]
step (U, pos) '\\' = [(L, move pos L)]
step (L, pos) '\\' = [(U, move pos U)]
step (D, pos) '\\' = [(R, move pos R)]
step (R, pos) '\\' = [(D, move pos D)]
step _ t = error $ "Invalid tile value: " ++ show t

processBeams :: Input -> [BeamState] -> State Energized [BeamState]
processBeams _ [] = return []
processBeams input beams = do
  nextBeams <- fmap concat $ flip S.mapM beams $ \beam@(dir, pos) -> do
    let tileMaybe = input Mat.!? pos
    energized <- S.get

    case tileMaybe of
      -- If the beam is outside of the grid, then we stop processing it
      Nothing -> return []
      Just tile -> do
        case energized M.!? pos of
          -- If a beam passing in the specific direction has already been processed at this position,
          -- it means we don't need to calculate it further (as it will have the same result we previously
          -- calculated).
          Just dirs -> if dir `elem` dirs then return [] else getNextBeams ()
          Nothing -> getNextBeams ()
       where
        getNextBeams :: () -> State Energized [BeamState]
        getNextBeams _ = do
          S.modify $ M.alter (Just . maybe [] (dir :)) pos
          return $ step beam tile

  processBeams input nextBeams

part1 :: Input -> Int
part1 input =
  let (_, energizedTiles) = S.runState (processBeams input [(R, (0, 0))]) M.empty
   in M.size energizedTiles

part2 :: Input -> Int
part2 input =
  let maxX = Mat.width input - 1
      maxY = Mat.height input - 1
      starts =
        [(D, pos) | x <- [0 .. maxX], let pos = (x, 0)]
          <> [(U, pos) | x <- [0 .. maxX], let pos = (x, maxY)]
          <> [(R, pos) | y <- [0 .. maxY], let pos = (0, y)]
          <> [(L, pos) | y <- [0 .. maxY], let pos = (maxX, y)]
   in maximum $ map (M.size . snd . (\x -> S.runState (processBeams input [x]) M.empty)) starts

day16 :: IO ()
day16 = runSolution "16" parser part1 part2
