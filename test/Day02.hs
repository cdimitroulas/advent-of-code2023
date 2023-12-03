{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day02 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day02 (parser, part1, part2)

exampleInput :: String
exampleInput = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

test_day02 :: TestTree
test_day02 = testGroup "Day02"
  [
    testCase "part 1 - example input" $ do
      part1 <$> parser exampleInput @?= Right 8,

    testCase "part 1" $ do
      input <- parser <$> readFile "data/day02.txt"
      part1 <$> input @?= Right 2632,

    testCase "part 2 - example input" $ do
      part2 <$> parser exampleInput @?= Right 2286,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day02.txt"
      part2 <$> input @?= Right 69629
  ]

