module Solutions.Day08 (day08, parser, part1, part2) where

import Control.Monad (MonadPlus (..))
import Control.Monad.ST
import qualified Data.Attoparsec.Text as P
import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as M
import Data.STRef.Strict
import qualified Data.Text as T
import Debug.Trace
import Lib.AOC (runSolution)
import Lib.Parsing (linesOf, skipRestOfLine)
import Lib.List (setAt, (!?))
import GHC.Base (when)
import Data.Maybe (isJust, catMaybes, fromJust)

type Input = (String, Map String (String, String))

-- Parsing
parser :: String -> Either String Input
parser input =
  P.parseOnly
    ( do
        directions <- P.takeWhile (not . P.isEndOfLine) <* P.endOfLine
        skipRestOfLine
        mapLines <- linesOf mapLineParser
        return (T.unpack directions, M.fromList mapLines)
    )
    (T.pack input)

mapLineParser :: P.Parser (String, (String, String))
mapLineParser = do
  node <- P.take 3
  P.string " = "
  leftNode <- P.string "(" *> P.take 3 <* ", "
  rightNode <- P.take 3 <* P.string ")"
  return (T.unpack node, (T.unpack leftNode, T.unpack rightNode))

-- Part1
turn :: Char -> (String, String) -> String
turn 'L' = fst
turn 'R' = snd

-- Stolen this definition from Control.Monad.Loops
whileM' :: (Monad m) => m Bool -> m a -> m [a]
whileM' p f = go
 where
  go = do
    x <- p
    if x
      then do
        x' <- f
        xs <- go
        return (return x' `mplus` xs)
      else return mzero

part1 :: Input -> Int
part1 (directions, mapLines) = runST $ do
  let directions' = cycle directions
  stepCounterRef <- newSTRef 0
  currentNodeRef <- newSTRef "AAA"

  let isNotAtEnd = (/= "ZZZ") <$> readSTRef currentNodeRef

  _ <- whileM' isNotAtEnd $ do
    stepCounter <- readSTRef stepCounterRef
    currentNode <- readSTRef currentNodeRef
    writeSTRef currentNodeRef (turn (directions' !! stepCounter) (mapLines M.! currentNode))
    modifySTRef stepCounterRef (+ 1)

  readSTRef stepCounterRef

lcmFold :: [Integer] -> Integer
lcmFold = foldl1 lcm

part2 :: Input -> Integer
part2 (directions, mapLines) = runST $ do
  let directions' = cycle directions
  let firstNodes = filter ((== 'A') . last) (M.keys mapLines)

  stepCounterRef <- newSTRef 0
  currentNodesRef <- newSTRef firstNodes
  -- cyclesToEnd is used to track how often each path reaches an end node
  -- we track the first time it reaches an end node and also the second time.
  -- The difference between those two values is assumed to be cyclic.
  --
  -- Using the cycles for each path (we have 1 path per starting node) 
  -- we can then calculate the Lowest Common Multiple which gives us the final answer
  cyclesToEndRef <- newSTRef (replicate (length firstNodes) (Nothing, Nothing))

  let hasCalculatedCycles = all (\(s, e) -> isJust s && isJust e) <$> readSTRef cyclesToEndRef

  _ <- whileM' (not <$> hasCalculatedCycles) $ do
    stepCounter <- readSTRef stepCounterRef
    currentNodes <- readSTRef currentNodesRef

    let nextDir = directions' !! fromInteger stepCounter
    let nextNodes =
          map
            (\currentNode -> turn nextDir (mapLines M.! currentNode))
            currentNodes

    mapM_
      ( \(node, idx) ->
        when ((== 'Z') $ last node) $
          modifySTRef cyclesToEndRef (\cycles -> case cycles !? idx of
            Just (Just start, Nothing) -> setAt cycles idx (Just start, Just stepCounter)
            Just (Nothing, Nothing) -> setAt cycles idx (Just stepCounter, Nothing)
            _ -> cycles
          )
      )
      (zip nextNodes [0 ..])

    writeSTRef currentNodesRef nextNodes
    modifySTRef stepCounterRef (+ 1)

  cycles <- readSTRef cyclesToEndRef
  return $ lcmFold (map (\(start, end) -> fromJust end - fromJust start) cycles)

day08 :: IO ()
day08 = runSolution "08" parser (fmap part1) (fmap part2)
