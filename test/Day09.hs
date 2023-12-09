{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day09 where

import Solutions.Day09 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = "0 3 6 9 12 15\n\
               \1 3 6 10 15 21\n\
               \10 13 16 21 30 45"

test_day09 :: TestTree
test_day09 =
  testGroup
    "Day09"
    [ testCase "parser" $ do
        parser exampleInput @?= [[0, 3, 6, 9, 12, 15], [1, 3, 6, 10, 15, 21], [10, 13, 16, 21, 30, 45]]

    , testCase "part 1 - example input" $ do
        part1 (parser exampleInput) @?= 114

    , testCase "part 1" $ do
        input <- parser <$> readFile "data/day09.txt"
        part1 input @?= 1953784198

    , testCase "part 2 - example input" $ do
        part2 (parser exampleInput) @?= 2

    , testCase "part 2" $ do
        input <- parser <$> readFile "data/day09.txt"
        part2 input @?= 957
    ]
