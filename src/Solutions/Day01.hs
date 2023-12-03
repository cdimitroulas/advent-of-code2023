module Solutions.Day01 (day01, parser, part1, part2) where

import Data.Char (isDigit, digitToInt)
import Lib.AOC (runSolution)

type Input = [String]

day01 :: IO ()
day01 = runSolution "01" lines part1 part2

parser :: String -> Input
parser = lines

part1 :: Input -> Int
part1 = sum . map (firstAndLast . map digitToInt . filter isDigit)

part2 :: Input -> Int
part2 = sum . map parseDigits

firstAndLast :: [Int] -> Int
firstAndLast x = 10 * head x + last x

parseDigits :: String -> Int
parseDigits = firstAndLast . map digitToInt . parseDigits'
  where
    parseDigits' [] = []
    parseDigits' ('o':'n':'e':xs) = "1" <> parseDigits' ("ne" <> xs)
    parseDigits' ('t':'w':'o':xs) = "2" <> parseDigits' ("wo" <> xs)
    parseDigits' ('t':'h':'r':'e':'e':xs) = "3" <> parseDigits' ("hree" <> xs)
    parseDigits' ('f':'o':'u':'r':xs) = "4" <> parseDigits' ("our" <> xs)
    parseDigits' ('f':'i':'v':'e':xs) = "5" <> parseDigits' ("ive" <> xs)
    parseDigits' ('s':'i':'x':xs) = "6" <> parseDigits' ("ix" <> xs)
    parseDigits' ('s':'e':'v':'e':'n':xs) = "7" <> parseDigits' ("even" <> xs)
    parseDigits' ('e':'i':'g':'h':'t':xs) = "8" <> parseDigits' ("ight" <> xs)
    parseDigits' ('n':'i':'n':'e':xs) = "9" <> parseDigits' ("ine" <> xs)
    parseDigits' (x:xs) = if isDigit x then x : parseDigits' xs else parseDigits' xs

-- parseDigits :: Text -> Int
-- parseDigits txt = firstAndLast $ catMaybes $ fromRight [] $ P.parseOnly (P.many1 digitParser) txt
--   where

-- TODO: interested to find out if there's a way to actually get this attoparsec parser to work. Currently
-- this doesn't work when there are overlapping strings like "oneight" which should be both 1 and 8
-- digitParser :: P.Parser (Maybe Int)
-- digitParser = Just <$> P.decimal
--       <|> Just 1 <$ P.string "one"
--       <|> Just 2 <$ P.string "two"
--       <|> Just 3 <$ P.string "three"
--       <|> Just 4 <$ P.string "four"
--       <|> Just 5 <$ P.string "five"
--       <|> Just 6 <$ P.string "six"
--       <|> Just 7 <$ P.string "seven"
--       <|> Just 8 <$ P.string "eight"
--       <|> Just 9 <$ P.string "nine"
--       <|> Nothing <$ P.anyChar

