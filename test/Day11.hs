{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day11 where

import qualified Data.Sequence as S
import Solutions.Day11 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = "...#......\n\
               \.......#..\n\
               \#.........\n\
               \..........\n\
               \......#...\n\
               \.#........\n\
               \.........#\n\
               \..........\n\
               \.......#..\n\
               \#...#....."

test_day11 :: TestTree
test_day11 =
  testGroup
    "Day11"
    [ testCase "parser" $ do
        parser exampleInput @?= 
          S.fromList [ 
              S.fromList ['.', '.', '.', '#', '.','.','.','.','.','.']
            , S.fromList ['.', '.', '.', '.', '.','.','.','#','.','.']
            , S.fromList ['#', '.', '.', '.', '.','.','.','.','.','.']
            , S.fromList ['.', '.', '.', '.', '.','.','.','.','.','.']
            , S.fromList ['.', '.', '.', '.', '.','.','#','.','.','.']
            , S.fromList ['.', '#', '.', '.', '.','.','.','.','.','.']
            , S.fromList ['.', '.', '.', '.', '.','.','.','.','.','#']
            , S.fromList ['.', '.', '.', '.', '.','.','.','.','.','.']
            , S.fromList ['.', '.', '.', '.', '.','.','.','#','.','.']
            , S.fromList ['#', '.', '.', '.', '#','.','.','.','.','.']
          ]

    , testCase "part 1 - example input" $ do
        part1 (parser exampleInput) @?= 374

    , testCase "part 1" $ do
        input <- parser <$> readFile "data/day11.txt"
        part1 input @?= 9445168

    -- , testCase "part 2 - example input" $ do
    --     part2 (parser exampleInput) @?= 2

    -- , testCase "part 2" $ do
    --     input <- parser <$> readFile "data/day11.txt"
    --     part2 input @?= 957
    ]
