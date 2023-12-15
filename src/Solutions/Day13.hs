module Solutions.Day13 (day13, parser, part1, part2) where

import Data.Foldable (toList)
import Data.List (partition, findIndices, find)
import Data.List.Split (splitOn)
import qualified Data.Sequence as S
import Lib.AOC (runSolution)
import Lib.Matrix (Matrix)
import qualified Lib.Matrix as Mat
import Data.Bifunctor (Bifunctor(bimap))

type Input = [[String]]

data ReflectionType = Vertical | Horizontal deriving (Show, Eq)

isVertical :: ReflectionType -> Bool
isVertical Vertical = True
isVertical _ = False

parser :: String -> Input
parser = map lines . splitOn "\n\n"

pairs :: (Eq a) => [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x1 : x2 : xs) = (x1, x2) : pairs (x2 : xs)

isReflectionIdx :: [String] -> Int -> Bool
isReflectionIdx list idx | idx >= length list - 1 = False
isReflectionIdx list idx =
  let (l, r) = splitAt (idx + 1) list
      minSublength = min (length l) (length r)
  in  take minSublength (reverse l) == take minSublength r

findReflectionIndex :: Matrix Char -> (Int, ReflectionType)
findReflectionIndex mat = case find (isReflectionIdx $ Mat.toList mat) potentialHorizIdxs of
  Just idx -> (idx, Horizontal)
  Nothing -> 
    case find (isReflectionIdx $ toList cols) potentialVertIdxs of
      Just idx -> (idx, Vertical)
      Nothing -> error "could not find reflection"
 where
  cols = map toList $ Mat.getCols mat
  colPairs = pairs cols
  rowPairs = pairs (toList mat)
  potentialHorizIdxs = findIndices (uncurry (==)) rowPairs
  potentialVertIdxs = findIndices (uncurry (==)) colPairs

part1 :: Input -> Int
part1 input =
  (\(colIdxs, rowIdxs) -> sum colIdxs + (100 * sum rowIdxs))
  $ bimap (map ((+ 1) . fst)) (map ((+ 1) . fst))
  $ partition (isVertical . snd)
  $ map findReflectionIndex matrices
 where
  matrices = map (S.fromList . map S.fromList) input

strDiff :: String -> String -> String
strDiff [] [] = ""
strDiff [] _ = ""
strDiff _ [] = ""
strDiff (c1:c1s) (c2:c2s) = if c1 == c2 then strDiff c1s c2s else c1 : strDiff c1s c2s

isSmudgeIdx :: [String] -> Int -> Bool
isSmudgeIdx list idx | idx >= length list - 1 = False
isSmudgeIdx list idx =
  let (l, r) = splitAt (idx + 1) list
      diff = concatMap (uncurry strDiff) (zip (reverse l) r)
  in  length diff == 1

findSmudgeIndex :: Matrix Char -> (Int, ReflectionType)
findSmudgeIndex mat = case find (isSmudgeIdx $ Mat.toList mat) [0 .. Mat.height mat - 1] of
  Just idx -> (idx, Horizontal)
  Nothing -> 
    case find (isSmudgeIdx $ toList cols) [0 .. Mat.width mat - 1] of
      Just idx -> (idx, Vertical)
      Nothing -> error "could not find smudge"
 where
  cols = map toList $ Mat.getCols mat


part2 :: Input -> Int
part2 input = 
  (\(colIdxs, rowIdxs) -> sum colIdxs + (100 * sum rowIdxs))
  $ bimap (map ((+ 1) . fst)) (map ((+ 1) . fst))
  $ partition (isVertical . snd)
  $ map findSmudgeIndex matrices
 where
  matrices = map (S.fromList . map S.fromList) input

day13 :: IO ()
day13 = runSolution "13" parser part1 part2
