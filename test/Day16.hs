{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day16 where

import Solutions.Day16 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput :: String
exampleInput = ".|...\\....\n\
               \|.-.\\.....\n\
               \.....|-...\n\
               \........|.\n\
               \..........\n\
               \.........\\\n\
               \..../.\\\\..\n\
               \.-.-/..|..\n\
               \.|....-|.\\\n\
               \..//.|...."

test_day16 :: TestTree
test_day16 =
  testGroup
    "Day16"
    [ 
      testCase "part 1 - example input" $ do
          part1 (parser exampleInput) @?= 46

    , testCase "part 2 - example input" $ do
        part2 (parser exampleInput) @?= 51
    ]
