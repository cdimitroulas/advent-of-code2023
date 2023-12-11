module Solutions.Day03 (day03, parser, part1, part2) where

import Control.Applicative
import qualified Data.Attoparsec.Text as P
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Lib.AOC (runSolution)
import qualified Lib.Matrix as Matrix
import Lib.Parsing (linesOf)
import Lib.Matrix (Matrix)
import Data.Foldable (toList)

data SchematicItem = Number Int | Symbol Char deriving (Eq, Show)

isSymbol :: SchematicItem -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isNumber :: SchematicItem -> Bool
isNumber (Number _) = True
isNumber _ = False

getNumberVal :: SchematicItem -> Int
getNumberVal (Number x) = x
getNumberVal _ = error "Tried to extract number from non-Number SchematicItem"

type Input = Matrix (Maybe SchematicItem)

day03 :: IO ()
day03 = runSolution "03" parser (fmap part1) (fmap part2)

parser :: String -> Either String Input
parser = fmap (fmap combineNumbers) . P.parseOnly (S.fromList <$> linesOf (S.fromList <$> P.many1 schematicItemParser)) . T.pack

part1 :: Input -> Int
part1 input =
  let partNumberPositions = getPartNumberPositions input
      partNumbers = map getNumberVal $ catMaybes $ removeConseqDups $ map (input Matrix.!) partNumberPositions
   in sum partNumbers

schematicItemParser :: P.Parser (Maybe SchematicItem)
schematicItemParser =
  (P.char '.' $> Nothing)
    <|> (Just . Number . digitToInt <$> P.digit)
    <|> (Just . Symbol <$> P.notChar '\n')

-- Combines the individual digits into larger numbers
combineNumbers :: Seq (Maybe SchematicItem) -> Seq (Maybe SchematicItem)
combineNumbers = S.fromList . combineNumbers' . toList
  where
    isNumber' (Just x) = isNumber x
    isNumber' _ = False

    combineNumbers' [] = []
    combineNumbers' ((Just (Number x1)) : xs) =
      let conseqNums = map (getNumberVal . fromJust) $ takeWhile isNumber' xs
          combinedNum = read $ concatMap show (x1 : conseqNums)
       in replicate (length conseqNums + 1) (Just (Number combinedNum)) <> combineNumbers' (drop (length conseqNums) xs)
    combineNumbers' (x : xs) = x : combineNumbers' xs

getSymbolPositions :: Input -> [(Int, Int)]
getSymbolPositions mat = do
  yPosition <- [0 .. Matrix.height mat - 1]
  xPosition <- toList $ S.findIndicesL (fromMaybe False . (Just isSymbol <*>)) (mat `S.index` yPosition)
  return (xPosition, yPosition)

getPartNumberPositions :: Input -> [(Int, Int)]
getPartNumberPositions = concatMap neighborPositions . getSymbolPositions

neighborPositions :: (Int, Int) -> [(Int, Int)]
neighborPositions (x, y) =
  [ (x + 1, y),
    (x + 1, y - 1),
    (x, y - 1),
    (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

removeConseqDups :: (Eq a) => [a] -> [a]
removeConseqDups [] = []
removeConseqDups (x1 : x2 : xs) = if x1 == x2 then removeConseqDups (x2 : xs) else x1 : removeConseqDups (x2 : xs)
removeConseqDups (x : xs) = x : removeConseqDups xs

part2 :: Input -> Int
part2 input = sum gearRatios
  where
    gearRatios :: [Int]
    gearRatios = map (product . getGearAdjacentNums) $ filter isGear potentialGearPositions

    potentialGearPositions = do
      yPosition <- [0 .. Matrix.height input - 1]
      xPosition <- toList $ S.findIndicesL (== Just (Symbol '*')) (input `S.index` yPosition)
      return (xPosition, yPosition)

    getGearAdjacentNums = map getNumberVal . filter isNumber . catMaybes . removeConseqDups . map (input Matrix.!) . neighborPositions

    isGear :: (Int, Int) -> Bool
    isGear gearPos = length (getGearAdjacentNums gearPos) == 2
