module Lib.AOC (runSolution) where

import           Data.Text    (Text)
import qualified Data.Text.IO as TIO

runSolution ::
  Show output =>
  String
  -- ^ the day number as a string (e.g. "01" or "11")
  -> (Text -> output)
  -- ^ the part1 solver fn
  -> (Text -> output)
  -- ^ the part2 solver fn
  -> IO ()
runSolution day part1 part2 = do
  input <- TIO.readFile ("data/day" ++ day ++ ".txt")
  putStrLn $ "Day " ++ day
  putStrLn "------------"

  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

  putStrLn "============"
