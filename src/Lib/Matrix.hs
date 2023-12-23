module Lib.Matrix (
  Matrix,
  (!),
  (!?),
  height,
  width,
  insertCol,
  insertRow,
  getRow,
  getCol,
  getCols,
  findColIndices,
  findRowIndices,
  Lib.Matrix.toList,
  findLocations,
  Lib.Matrix.map,
  updateRow,
  updateCol,
  toArray,
  fromList,
  transpose,
  mapCols,
  mapRows
) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.List (findIndices)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Lib.List (mapWithIndex)
import Prelude hiding (elem)

type Matrix a = Seq (Seq a)

type Position = (Int, Int) -- x, y

(!) :: Matrix a -> Position -> a
mat ! (x, y) = mat `S.index` y `S.index` x

(!?) :: Position -> Matrix a -> Maybe a
(!?) (x, y) mat = mat S.!? y >>= (S.!? x)

height :: Matrix a -> Int
height = S.length

width :: Matrix a -> Int
width = S.length . (`S.index` 0)

insertCol :: Seq a -> Int -> Matrix a -> Matrix a
insertCol col colIdx = fmap (S.insertAt colIdx (col `S.index` colIdx))

updateCol :: Seq a -> Int -> Matrix a -> Matrix a
updateCol col colIdx = fmap (S.update colIdx (col `S.index` colIdx))

insertRow :: Seq a -> Int -> Matrix a -> Matrix a
insertRow row rowIdx = S.insertAt rowIdx row

updateRow :: Seq a -> Int -> Matrix a -> Matrix a
updateRow row rowIdx = S.update rowIdx row

getRow :: Int -> Matrix a -> Seq a
getRow = flip S.index

getCol :: Int -> Matrix a -> Seq a
getCol colIdx = fmap (`S.index` colIdx)

getCols :: Matrix a -> [Seq a]
getCols mat = [getCol colIdx mat | colIdx <- [0 .. width mat - 1]]

findColIndices :: (Seq a -> Bool) -> Matrix a -> [Int]
findColIndices predicate mat = findIndices predicate (getCols mat)

findRowIndices :: (Seq a -> Bool) -> Matrix a -> [Int]
findRowIndices = S.findIndicesL

toList :: Matrix a -> [[a]]
toList = Data.Foldable.toList . fmap Data.Foldable.toList

fromList :: [[a]] -> Matrix a
fromList = S.fromList . fmap S.fromList

toArray :: Matrix a -> Array (Int, Int) a
toArray mat = A.array ((0, 0), fst $ last elemsWithPositions) elemsWithPositions
 where
  elemsWithPositions = concat $ mapWithIndex (\y line -> mapWithIndex (\x c -> ((x, y), c)) line) $ Lib.Matrix.toList mat

findLocations :: (a -> Bool) -> Matrix a -> [(Int, Int)]
findLocations predicate mat = [(x, y) | y <- [0 .. height mat - 1], x <- [0 .. width mat - 1], predicate (mat ! (x, y))]

-- Swaps columns/rows
transpose :: Matrix a -> Matrix a
transpose = S.fromList . getCols

map :: (a -> b) -> Matrix a -> Matrix b
map f = fmap (fmap f)

mapCols :: ([a] -> [b]) -> Matrix a -> Matrix b
mapCols f mat = transpose $ S.fromList $ Prelude.map (S.fromList . f . Data.Foldable.toList)  $ getCols mat

mapRows :: ([a] -> [b]) -> Matrix a -> Matrix b
mapRows f mat = S.fromList $ Prelude.map (S.fromList . f . Data.Foldable.toList) $ Data.Foldable.toList mat
