module Day01 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day01 (parser, part1, part2)

test_day01 :: TestTree
test_day01 = testGroup "Day 01 tests"
  [
    testCase "part 1" $ do
      input <- parser <$> readFile "data/day01.txt"
      part1 input @?= 54708,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day01.txt"
      part2 input @?= 54087
  ]

