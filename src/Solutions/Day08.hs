module Solutions.Day08 (day08, parser, part1, part2) where

import qualified Data.Attoparsec.Text as P
import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Lib.AOC (runSolution)
import Lib.Parsing (linesOf, skipRestOfLine)

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

runPath :: Input -> String -> Int -> Int
runPath _ node steps | last node == 'Z' = steps
runPath input@(directions, mapLines) node0 steps =
  let node1 = turn (directions !! steps) (mapLines M.! node0)
  in  runPath input node1 (steps + 1)

part1 :: Input -> Int
part1 (directions, mapLines) = runPath (cycle directions, mapLines) "AAA" 0

lcmFold :: [Integer] -> Integer
lcmFold = foldl1 lcm

part2 :: Input -> Integer
part2 (directions, mapLines) = lcmFold $ map runPath' firstNodes
  where
   firstNodes = filter ((== 'A') . last) (M.keys mapLines)
   runPath' n = toInteger $ runPath (cycle directions, mapLines) n 0

day08 :: IO ()
day08 = runSolution "08" parser (fmap part1) (fmap part2)
