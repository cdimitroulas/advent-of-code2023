{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day05 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day05 (MappingRange(..), parser, part1, part2)

exampleInput :: String
exampleInput = "seeds: 79 14 55 13\n\
                \\n\
                \seed-to-soil map:\n\
                \50 98 2\n\
                \52 50 48\n\
                \\n\
                \soil-to-fertilizer map:\n\
                \0 15 37\n\
                \37 52 2\n\
                \39 0 15\n\
                \\n\
                \fertilizer-to-water map:\n\
                \49 53 8\n\
                \0 11 42\n\
                \42 0 7\n\
                \57 7 4\n\
                \\n\
                \water-to-light map:\n\
                \88 18 7\n\
                \18 25 70\n\
                \\n\
                \light-to-temperature map:\n\
                \45 77 23\n\
                \81 45 19\n\
                \68 64 13\n\
                \\n\
                \temperature-to-humidity map:\n\
                \0 69 1\n\
                \1 0 69\n\
                \\n\
                \humidity-to-location map:\n\
                \60 56 37\n\
                \56 93 4"

test_day05 :: TestTree
test_day05 = testGroup "Day05"
  [
    testCase "parser" $ do
      parser exampleInput @?= 
        Right (
          [79, 14, 55, 13],
          [
            [MappingRange 50 98 2, MappingRange 52 50 48],
            [MappingRange 0 15 37, MappingRange 37 52 2, MappingRange 39 0 15],
            [MappingRange 49 53 8, MappingRange 0 11 42, MappingRange 42 0 7, MappingRange 57 7 4],
            [MappingRange 88 18 7, MappingRange 18 25 70],
            [MappingRange 45 77 23, MappingRange 81 45 19, MappingRange 68 64 13],
            [MappingRange 0 69 1, MappingRange 1 0 69],
            [MappingRange 60 56 37, MappingRange 56 93 4]
          ]),

    testCase "part 1 - example input" $ do
      part1 <$> parser exampleInput @?= Right 35,

    testCase "part 1" $ do
      input <- parser <$> readFile "data/day05.txt"
      part1 <$> input @?= Right 340994526,

    testCase "part 2 - example input" $ do
      part2 <$> parser exampleInput @?= Right 46,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day05.txt"
      part2 <$> input @?= Right 52210644
  ]

