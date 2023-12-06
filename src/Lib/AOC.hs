module Lib.AOC (runSolution) where

import System.Clock
import Formatting
import Formatting.Clock

runSolution ::
  Show output1 => Show output2 => 
  String
    -- ^ the day number as a string (e.g. "01" or "11")
  -> (String -> input)
  -- ^ parser
  -> (input -> output1)
  -- ^ the part1 solver fn
  -> (input -> output2)
  -- ^ the part2 solver fn
  -> IO ()
runSolution day parser part1 part2 = do
  input <- parser <$> readFile ("data/day" ++ day ++ ".txt")
  putStrLn $ "Day " ++ day
  putStrLn "------------"

  putStrLn "Part 1:"
  p1Start <- getTime Monotonic
  print $ part1 input
  p1End <- getTime Monotonic
  fprint (timeSpecs % "\n") p1Start p1End

  putStrLn "Part 2:"
  p2Start <- getTime Monotonic
  print $ part2 input
  p2End <- getTime Monotonic
  fprint (timeSpecs % "\n") p2Start p2End

  putStrLn "============"
