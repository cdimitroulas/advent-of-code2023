module Solutions.Day04 (day04, parser, part1, part2) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Lib.AOC (runSolution)
import Lib.Parsing (linesOf)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Foldable (foldl')

day04 :: IO ()
day04 = runSolution "04" parser (fmap part1) (fmap part2)

data Card = Card {cardNum :: Int, winningNums :: [Int], playerNums :: [Int], copies :: Int} deriving (Show)

type Input = [Card]

parser :: String -> Either String Input
parser = P.parseOnly (linesOf cardParser) . T.pack

cardParser :: P.Parser Card
cardParser =
  let numParser = read <$> (P.string "Card" *> P.many1 P.space *> P.many1 P.digit <* P.char ':')
      numberParser = P.many1 P.space *> (map read <$>  P.many1 P.digit `P.sepBy` P.many1 (P.char ' '))
   in Card <$> numParser <*> numberParser <* P.string " |" <*> numberParser <*> pure 1

part1 :: Input -> Int
part1 = sum . map cardPoints
  where
    cardPoints :: Card -> Int
    cardPoints card = if totalWinning card == 0 then 0 else 2 ^ (totalWinning card - 1)

totalWinning :: Card -> Int
totalWinning card = length $ filter (`elem` card.playerNums) card.winningNums

part2 :: Input -> Int
part2 input = M.foldl' (\total card -> total + card.copies) 0 $ createCopies (cardNum $ head input) $ toMap input
  where
    toMap :: [Card] -> Map Int Card
    toMap = foldl' (\m card -> M.insert card.cardNum card m) M.empty

    createCopies idx m =
      let currentCard = m M.! idx
      in if idx == cardNum (last input)
          then updateCardCounts currentCard m
          else createCopies (idx + 1) (updateCardCounts currentCard m)

    updateCardCounts :: Card -> Map Int Card -> Map Int Card
    updateCardCounts card m =
      let totalWon = totalWinning (m M.! card.cardNum)
          cardNumsToCopy = if totalWon == 0 then [] else [card.cardNum + 1 .. card.cardNum + totalWon]
      in foldl'
            (flip (M.update (\c -> Just $ c { copies = c.copies + (1 * card.copies) })))
            m
            cardNumsToCopy
