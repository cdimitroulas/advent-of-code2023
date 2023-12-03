module Solutions.Day02 (day02, parser, part1, part2) where

import Lib.AOC (runSolution)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Lib.Parsing (linesOf)
import qualified Data.Text as T
import Data.Either (fromRight)
import Control.Applicative
import Data.Text (Text)

data Colour = Red | Green | Blue deriving (Show)

strToColour :: Text -> Colour
strToColour "red" = Red
strToColour "blue" = Blue
strToColour "green" = Green
strToColour _ = error "invalid colour"

type GameInfoItem = (Colour, Int)
type GameInfo = [GameInfoItem]
type Game = (Int, [GameInfo])
type Input = [Game]

day02 :: IO ()
day02 = runSolution "02" parser (fmap part1) (fmap part2)

parser :: String -> Either String Input
parser = P.parseOnly inputParser . T.pack

inputParser :: Parser Input
inputParser = linesOf $ do
  gameId <- P.string "Game " *> P.many1 P.digit <* P.string ": "
  gameInfo <- (cubeCountParser `P.sepBy` ", ") `P.sepBy` "; "
  return (read gameId, gameInfo)

cubeCountParser :: Parser GameInfoItem
cubeCountParser = do
  count <- P.many1 P.digit
  P.space
  colour <- P.string "red" <|> P.string "green" <|> P.string "blue"
  return (strToColour colour, read count)

part1:: Input -> Int
part1 = sum . map fst . filter gameIsPossible
  where
    gameIsPossible (_, cubes) = all (all isPossibleCubeCount) cubes

    isPossibleCubeCount cubeCount =
      case cubeCount of
        (Red, count) -> count <= 12
        (Green, count) -> count <= 13
        (Blue, count) -> count <= 14


part2 :: Input -> Int
part2 = sum . map (product . findMinGameCubes)
  where
    findMinGameCubes :: Game -> [Int]
    findMinGameCubes (_, gameInfo) = 
      foldr 
        (\(colour, count) [redCount, greenCount, blueCount] -> case colour of
            Red -> [max redCount count, greenCount, blueCount]
            Blue -> [redCount, greenCount, max blueCount count]
            Green -> [redCount, max greenCount count, blueCount]
          ) 
        [0, 0, 0] 
        (concat gameInfo)


