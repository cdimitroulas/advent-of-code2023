module Solutions.Day05 (MappingRange (..), parser, part1, part2, day05) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Attoparsec.Text as P
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import Data.STRef
import qualified Data.Text as T
import Lib.AOC (runSolution)
import Lib.Parsing (linesOf, number, skipRestOfLine, spaces)

data MappingRange = MappingRange {destRangeStart :: Integer, srcRangeStart :: Integer, rangeLength :: Integer}
  deriving (Eq, Show)

type Mapping = [MappingRange]

type Input = ([Integer], [Mapping])

-- Parsing
parser :: String -> Either String Input
parser txt = flip P.parseOnly (T.pack txt) $ do
  seeds <- seedsParser
  mappings <- mappingsParser
  return (seeds, mappings)

seedsParser :: P.Parser [Integer]
seedsParser = P.string "seeds: " *> (number `P.sepBy` spaces) <* skipRestOfLine <* skipRestOfLine

mappingRangeParser :: P.Parser MappingRange
mappingRangeParser = MappingRange <$> number <* spaces <*> number <* spaces <*> number

mappingsParser :: P.Parser [Mapping]
mappingsParser = linesOf (mappingParser <* (P.endOfInput <|> P.endOfLine))
 where
  mappingParser = skipRestOfLine *> linesOf mappingRangeParser

-- Part 1
mappingRangeToFn :: MappingRange -> (Integer -> Maybe Integer)
mappingRangeToFn MappingRange{..} src =
  if srcRangeStart <= src && src < srcRangeStart + rangeLength
    then Just (destRangeStart + (src - srcRangeStart))
    else Nothing

mappingToFn :: Mapping -> (Integer -> Integer)
mappingToFn mapping src =
  fromMaybe src
    $ foldl'
      (\result mappingRange -> if isJust result then result else mappingRangeToFn mappingRange src)
      Nothing
      mapping

mapSeeds :: Input -> [Integer]
mapSeeds (seeds, mappings) =
  let mappingFns = map mappingToFn mappings
   in map ((flip $ foldl' (&)) mappingFns) seeds

part1 :: Input -> Integer
part1 = minimum . mapSeeds

-- Part 2
type Range = (Integer, Integer)

isValidRange :: Range -> Bool
isValidRange (x1, x2) = x1 <= x2

rangeOverlap :: Range -> Range -> Maybe Range
rangeOverlap (x1, x2) (y1, y2)
  | x2 < y1 || y2 < x1 = Nothing
  | otherwise = Just (max x1 y1, min x2 y2)

mappingRangeToFn' :: MappingRange -> (Range -> Maybe (Range, [Range]))
mappingRangeToFn' MappingRange{..} src@(x1, x2) =
  let mapValue v = destRangeStart + (v - srcRangeStart)
   in case rangeOverlap src (srcRangeStart, srcRangeStart + rangeLength - 1) of
        -- Returns a tuple where the first element is the mapped range and the snd element is a list of unmapped ranges
        Just (oStart, oEnd) ->
          Just ((mapValue oStart, mapValue oEnd), filter isValidRange $ (x1, oStart - 1) : [(oEnd + 1, x2)])
        Nothing -> Nothing

mappingToFn' :: Mapping -> (Range -> [Range])
mappingToFn' mapping src = runST $ do
  rangesToMapRef <- newSTRef [src]
  mappedRangesRef <- newSTRef []

  forM_ mapping $ \mappingRange -> do
    rangesToMap <- readSTRef rangesToMapRef

    forM_ rangesToMap $ \range -> do
      let mapResult = mappingRangeToFn' mappingRange range

      -- If the mapping range produces a result, then we take the first item of that tuple result and put it into
      -- the mappedRanges list. The second item of the tuple result we put back into the rangesToMap list as those
      -- are items which couldn't be mapped and we need to try running them through the rest of the mapping ranges.
      --
      -- If the mapping range produces no result, we do nothing which will result in the same ranges being tried on
      -- subsequent mapping ranges.
      ( case mapResult of
          Just (mappedRange, unmappedRanges) -> do
            _ <- modifySTRef mappedRangesRef (<> [mappedRange])
            modifySTRef rangesToMapRef (const unmappedRanges)
          Nothing -> return ()
        )

  mappedRanges <- readSTRef mappedRangesRef
  unmappedRanges <- readSTRef rangesToMapRef
  return (mappedRanges <> unmappedRanges)

mapSeedRange :: [Range -> [Range]] -> Range -> [Range]
mapSeedRange mappingFns seed = foldl' (flip concatMap) [seed] mappingFns

mapSeedRanges :: ([Range], [Mapping]) -> [(Integer, Integer)]
mapSeedRanges (seeds, mappings) =
  let mappingFns = map mappingToFn' mappings
   in concatMap (mapSeedRange mappingFns) seeds

part2 :: Input -> Integer
part2 (seeds, mappings) = minimum $ map getLowerBound $ mapSeedRanges (toRanges seeds, mappings)
 where
  getLowerBound = fst

toRanges :: [Integer] -> [Range]
toRanges [] = []
toRanges [_] = error "Invalid list provided to toRanges. Must have an even number of elems"
toRanges (x1 : x2 : xs) = (x1, x1 + x2 - 1) : toRanges xs

day05 :: IO ()
day05 = runSolution "05" parser (fmap part1) (fmap part2)
