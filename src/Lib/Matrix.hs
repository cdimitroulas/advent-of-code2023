module Lib.Matrix (Matrix, (!), (!?), height, width, insertCol, insertRow, getRow, getCol, getCols, findColIndices, findRowIndices, Lib.Matrix.toList, findLocations) where

import Data.Foldable (toList)
import Data.List (findIndices)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
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

insertRow :: Seq a -> Int -> Matrix a -> Matrix a
insertRow row rowIdx = S.insertAt rowIdx row

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
toList mat = Data.Foldable.toList $ fmap Data.Foldable.toList mat

findLocations :: (a -> Bool) -> Matrix a -> [(Int, Int)]
findLocations predicate mat = [(x, y) | y <- [0 .. height mat - 1], x <- [0 .. width mat - 1], predicate (mat ! (x, y))]
