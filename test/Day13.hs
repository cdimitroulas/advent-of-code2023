{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day13 where

import Solutions.Day13 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = "#.##..##.\n\
               \..#.##.#.\n\
               \##......#\n\
               \##......#\n\
               \..#.##.#.\n\
               \..##..##.\n\
               \#.#.##.#.\n\
               \\n\
               \#...##..#\n\
               \#....#..#\n\
               \..##..###\n\
               \#####.##.\n\
               \#####.##.\n\
               \..##..###\n\
               \#....#..#"

test_day13 :: TestTree
test_day13 =
  testGroup
    "Day13"
    [ 
      testCase "parser" $ do
        parser exampleInput @?= 
          [
            [
                "#.##..##."
              , "..#.##.#."
              , "##......#"
              , "##......#"
              , "..#.##.#."
              , "..##..##."
              , "#.#.##.#."
            ],
            [
                "#...##..#"
              , "#....#..#"
              , "..##..###"
              , "#####.##."
              , "#####.##."
              , "..##..###"
              , "#....#..#"
            ]
          ]

    , testCase "part 1 - example input" $ do
        part1 (parser exampleInput) @?= 405

    , testCase "part 1" $ do
        input <- parser <$> readFile "data/day13.txt"
        part1 input @?= 27664

    , testCase "part 2 - example input" $ do
        part2 (parser exampleInput) @?= 400

    , testCase "part 2" $ do
        input <- parser <$> readFile "data/day13.txt"
        part2 input @?= 33991
    ]
