{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day11 where

import qualified Data.Sequence as S
import Solutions.Day11 (parser, part1, part2, sumOfGalaxyDistances)
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

    , testCase "part 2 - example input" $ do
        sumOfGalaxyDistances 10 (parser exampleInput) @?= 1030

    , testCase "part 2 - example input (factor 100)" $ do
        sumOfGalaxyDistances 100 (parser exampleInput) @?= 8410 

    -- , testCase "part 2" $ do
    --     input <- parser <$> readFile "data/day11.txt"
    --     part2 input @?= 100 -- TODO: get real value
    ]
