{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day08 where

import qualified Data.Map.Strict as M
import Solutions.Day08 (parser, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

exampleInput1 :: String
exampleInput1 =
  "RL\n\
  \\n\
  \AAA = (BBB, CCC)\n\
  \BBB = (DDD, EEE)\n\
  \CCC = (ZZZ, GGG)\n\
  \DDD = (DDD, DDD)\n\
  \EEE = (EEE, EEE)\n\
  \GGG = (GGG, GGG)\n\
  \ZZZ = (ZZZ, ZZZ)"

exampleInput2 :: String
exampleInput2 =
  "LR\n\
  \\n\
  \11A = (11B, XXX)\n\
  \11B = (XXX, 11Z)\n\
  \11Z = (11B, XXX)\n\
  \22A = (22B, XXX)\n\
  \22B = (22C, 22C)\n\
  \22C = (22Z, 22Z)\n\
  \22Z = (22B, 22B)\n\
  \XXX = (XXX, XXX)"

test_day08 :: TestTree
test_day08 =
  testGroup
    "Day08"
    [ testCase "parser" $ do
        parser exampleInput1
          @?= Right
            ( "RL"
            , M.fromList
                [ ("AAA", ("BBB", "CCC"))
                , ("BBB", ("DDD", "EEE"))
                , ("CCC", ("ZZZ", "GGG"))
                , ("DDD", ("DDD", "DDD"))
                , ("EEE", ("EEE", "EEE"))
                , ("GGG", ("GGG", "GGG"))
                , ("ZZZ", ("ZZZ", "ZZZ"))
                ]
            )

    , testCase "part 1 - example input" $ do
        part1 <$> parser exampleInput1 @?= Right 2

    , testCase "part 1" $ do
        input <- parser <$> readFile "data/day08.txt"
        part1 <$> input @?= Right 19667

    , testCase "part 2 - example input" $ do
        part2 <$> parser exampleInput2 @?= Right 6

    , testCase "part 2" $ do
        input <- parser <$> readFile "data/day08.txt"
        part2 <$> input @?= Right 19185263738117
    ]
