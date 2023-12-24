module Solutions.Day16 (day16, parser, part1, part2) where

import Lib.AOC (runSolution)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (State)
import qualified Control.Monad.State as S

data Tile = Empty | Vsplit | Hsplit | Fslash | Bslash deriving (Show, Eq)

tileFromChar :: Char -> Tile
tileFromChar '.' = Empty
tileFromChar '|' = Vsplit
tileFromChar '-' = Hsplit
tileFromChar '/' = Fslash
tileFromChar '\\' = Bslash
tileFromChar t = error $ "Invalid tile: " ++ [t]

data Direction = Up | Down | Left' | Right' deriving (Show, Eq)

isVerticalDir :: Direction -> Bool
isVerticalDir dir
  | dir == Up || dir == Down = True
  | otherwise = False

isHorizontalDir :: Direction -> Bool
isHorizontalDir = not . isVerticalDir

type Input = Matrix Tile

parser :: String -> Input
parser = Mat.fromList . map (map tileFromChar) . lines

type Energized = Map (Int, Int) [Direction]
type BeamState = (Direction, (Int, Int))

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) Up = (x, y - 1)
move (x, y) Down = (x, y + 1)
move (x, y) Left' = (x - 1, y)
move (x, y) Right' = (x + 1, y)

step :: BeamState -> Tile -> [BeamState]
step (dir, pos) Empty = [(dir, move pos dir)]
step (dir, pos) Vsplit
  | isVerticalDir dir = [(dir, move pos dir)]
  | otherwise = [(Up, move pos Up), (Down, move pos Down)]
step (dir, pos) Hsplit
  | isHorizontalDir dir = [(dir, move pos dir)]
  | otherwise = [(Left', move pos Left'), (Right', move pos Right')]
step (Up, pos) Fslash = [(Right', move pos Right')]
step (Left', pos) Fslash = [(Down, move pos Down)]
step (Down, pos) Fslash = [(Left', move pos Left')]
step (Right', pos) Fslash = [(Up, move pos Up)]
step (Up, pos) Bslash = [(Left', move pos Left')]
step (Left', pos) Bslash = [(Up, move pos Up)]
step (Down, pos) Bslash = [(Right', move pos Right')]
step (Right', pos) Bslash = [(Down, move pos Down)]

-- TODO: need to handle infinite beam bouncing, as sometimes beam can keep going around a certain cycle
-- forever.
-- Perhaps if the same BeamState has been processed multile times then we can stop?
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
            -- it means we don't need to calculate it further as it has already been calculated.
            Just dirs -> if dir `elem` dirs 
                         then return []
                         else do
                          S.modify $ M.alter (Just . maybe [] (dir :)) pos
                          return $ step beam tile
            -- TODO: how can I avoid repeating these two lines again below?
            Nothing -> do
                S.modify $ M.alter (Just . maybe [] (dir :)) pos
                return $ step beam tile


  processBeams input nextBeams

part1 :: Input -> Int
part1 input =
  let (_, energizedTiles) = S.runState (processBeams input [(Right', (0, 0))]) M.empty
  in M.size energizedTiles

part2 :: Input -> Int
part2 input = 
  let downInputs = map ((Down,) . (,0)) [0 .. Mat.width input - 1]
      upInputs = map ((Up,) . (, Mat.height input - 1)) [0 .. Mat.width input - 1]
      rightInputs = map ((Right',) . (0,)) [0 .. Mat.height input - 1]
      leftInputs = map ((Left',) . (Mat.width input - 1,)) [0 .. Mat.height input - 1]
      allInputs = concat [downInputs, upInputs, rightInputs, leftInputs]
  in maximum $ map (M.size . snd . (\x -> S.runState (processBeams input [x]) M.empty)) allInputs

day16 :: IO ()
day16 = runSolution "16" parser part1 part2
