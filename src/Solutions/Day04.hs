{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day04 (day04, parser, part1, part2) where

import qualified Data.Attoparsec.Text as P
import Data.Foldable (foldl', toList)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.AOC (runSolution)
import Lib.Parsing (linesOf, number, spaces)

data Card = Card {cardNum :: Int, winningNums :: Set Int, playerNums :: Set Int, copies :: Int} deriving (Show)

addCardCopies :: Int -> Card -> Card
addCardCopies copiesToAdd c = c{copies = c.copies + copiesToAdd}

type Input = [Card]

parser :: String -> Either String Input
parser = P.parseOnly (linesOf cardParser) . T.pack
 where
  numParser = P.string "Card" *> spaces *> number <* P.char ':'
  numberParser = S.fromList <$> (spaces *> number `P.sepBy` spaces)
  cardParser :: P.Parser Card
  cardParser = Card <$> numParser <*> (numberParser <* P.string " |") <*> numberParser <*> pure 1

part1 :: Input -> Int
part1 = sum . map (\card -> if winningNumCount card == 0 then 0 else 2 ^ (winningNumCount card - 1))

winningNumCount :: Card -> Int
winningNumCount card = length $ S.intersection card.winningNums card.playerNums

part2 :: Input -> Int
part2 input = sum $ map copies $ toList $ createCopies 0 $ Seq.fromList input
 where
  createCopies idx cardsList
    | idx == length cardsList - 1 = updatedCardCounts
    | otherwise = createCopies (idx + 1) updatedCardCounts
   where
    updatedCardCounts = updateCardCounts (cardsList `Seq.index` idx) cardsList

  updateCardCounts :: Card -> Seq.Seq Card -> Seq.Seq Card
  updateCardCounts card cardList =
    let totalWon = winningNumCount (cardList `Seq.index` (card.cardNum - 1))
     in foldl'
          (\accum cardIdx -> Seq.update cardIdx (addCardCopies card.copies (cardList `Seq.index` cardIdx)) accum)
          cardList
          [card.cardNum .. card.cardNum + totalWon - 1]

day04 :: IO ()
day04 = runSolution "04" parser (fmap part1) (fmap part2)
