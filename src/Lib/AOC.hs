module Lib.AOC (runSolution) where

runSolution ::
  Show output =>
  String
    -- ^ the day number as a string (e.g. "01" or "11")
  -> (String -> input)
  -- ^ parser
  -> (input -> output)
  -- ^ the part1 solver fn
  -> (input -> output)
  -- ^ the part2 solver fn
  -> IO ()
runSolution day parser part1 part2 = do
  input <- parser <$> readFile ("data/day" ++ day ++ ".txt")
  putStrLn $ "Day " ++ day
  putStrLn "------------"

  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

  putStrLn "============"
