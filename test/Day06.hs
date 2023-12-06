{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day06 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day06 (parser, part1, part2)

exampleInput :: String
exampleInput = "Time:      7  15   30\n\
               \Distance:  9  40  200"

test_day06 :: TestTree
test_day06 = testGroup "Day06"
  [
    testCase "parser" $ do
      parser exampleInput @?= [(7, 9), (15, 40), (30, 200)],

    testCase "part 1 - example input" $ do
      part1 (parser exampleInput) @?= 288,

    testCase "part 1" $ do
      input <- parser <$> readFile "data/day06.txt"
      part1 input @?= 138915,

    testCase "part 2 - example input" $ do
      part2 (parser exampleInput) @?= 71503,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day06.txt"
      part2 input @?= 27340847
  ]

