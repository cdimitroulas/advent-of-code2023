{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day14 where

import Solutions.Day14 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = "O....#....\n\
               \O.OO#....#\n\
               \.....##...\n\
               \OO.#O....O\n\
               \.O.....O#.\n\
               \O.#..O.#.#\n\
               \..O..#O..O\n\
               \.......O..\n\
               \#....###..\n\
               \#OO..#...."

test_day14 :: TestTree
test_day14 =
  testGroup
    "Day14"
    [ 
      -- testCase "parser" $ do
      --   parser exampleInput @?= []

    testCase "part 1 - example input" $ do
        part1 (parser exampleInput) @?= 136

    , testCase "part 2 - example input" $ do
        part2 (parser exampleInput) @?= 64
    ]
