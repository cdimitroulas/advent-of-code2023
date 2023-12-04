{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day03 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day03 (parser, part1, part2)

exampleInput :: String
exampleInput = "467..114..\n\
               \...*......\n\
               \..35..633.\n\
               \......#...\n\
               \617*......\n\
               \.....+.58.\n\
               \..592.....\n\
               \......755.\n\
               \...$.*....\n\
               \.664.598.."

test_day03 :: TestTree
test_day03 = testGroup "Day03"
  [
    testCase "part 1 - example input" $ do
      part1 <$> parser exampleInput @?= Right 4361,

    testCase "part 1" $ do
      input <- parser <$> readFile "data/day03.txt"
      part1 <$> input @?= Right 525181,

    testCase "part 2 - example input" $ do
      part2 <$> parser exampleInput @?= Right 467835,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day03.txt"
      part2 <$> input @?= Right 84289137
  ]

