module Lib.Matrix (Matrix, (!), (!?), height, width) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (elem)

type Matrix a = Vector (Vector a)

type Position = (Int, Int) -- x, y

(!) :: Position -> Matrix a -> a
(!) (x, y) mat = mat V.! y V.! x

(!?) :: Position -> Matrix a -> Maybe a
(!?) (x, y) mat = mat V.!? y >>= (V.!? x)

height :: Matrix a -> Int
height = V.length

width :: Matrix a -> Int
width = V.length . V.head
