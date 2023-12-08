{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day07 where

import Test.Tasty
import Test.Tasty.HUnit
import Solutions.Day07 (parser, part1, part2, Card(..))

exampleInput :: String
exampleInput = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

test_day07 :: TestTree
test_day07 = testGroup "Day07"
  [
    testCase "parser" $ do
      parser exampleInput @?= 
        [
          ([Three, Two, T, Three, K], 765)
          , ([T, Five, Five, J, Five], 684)
          , ([K, K, Six, Seven, Seven], 28)
          , ([K, T, J, J, T], 220)
          , ([Q, Q, Q, J, A], 483)
          ],

    testCase "part 1 - example input" $ do
      part1 (parser exampleInput) @?= 6440,

    testCase "part 1" $ do
      input <- parser <$> readFile "data/day07.txt"
      part1 input @?= 251545216,

    testCase "part 2 - example input" $ do
      part2 (parser exampleInput) @?= 5905,

    testCase "part 2" $ do
      input <- parser <$> readFile "data/day07.txt"
      part2 input @?= 250384185
  ]

