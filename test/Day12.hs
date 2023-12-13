{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day12 where

import Solutions.Day12 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = "???.### 1,1,3\n\
               \.??..??...?##. 1,1,3\n\
               \?#?#?#?#?#?#?#? 1,3,1,6\n\
               \????.#...#... 4,1,1\n\
               \????.######..#####. 1,6,5\n\
               \?###???????? 3,2,1"

test_day12 :: TestTree
test_day12 =
  testGroup
    "Day12"
    [ 
      testCase "parser" $ do
        parser exampleInput @?= 
          [
              ("???.###", [1, 1, 3])
            , (".??..??...?##.", [1, 1, 3])
            , ("?#?#?#?#?#?#?#?", [1, 3, 1, 6])
            , ("????.#...#...", [4, 1, 1])
            , ("????.######..#####.", [1, 6, 5])
            , ("?###????????", [3, 2, 1])
          ]

    , testCase "parser  - example with double digit numbers" $ do
        parser "?????????? 11,3" @?= [("??????????", [11, 3])]

    , testCase "part 1 - example input" $ do
        part1 (parser exampleInput) @?= 21

    -- , testCase "part 1" $ do
    --     input <- parser <$> readFile "data/day12.txt"
    --     part1 input @?= 9445168

    -- , testCase "part 2 - example input" $ do
    --     sumOfGalaxyDistances 10 (parser exampleInput) @?= 1030

    -- , testCase "part 2" $ do
    --     input <- parser <$> readFile "data/day12.txt"
    --     part2 input @?= 100 -- TODO: get real value
    ]
