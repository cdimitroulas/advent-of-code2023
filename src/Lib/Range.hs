module Lib.Range (Range(..), mkRange, getOverlap, map, start, end) where

import Prelude hiding (map)

newtype Range = Range (Integer, Integer)

mkRange :: (Integer, Integer) -> Maybe Range
mkRange r@(x1, x2) 
  | x1 <= x2 = Just $ Range r
  | otherwise = Nothing

getOverlap :: Range -> Range -> Maybe Range
getOverlap (Range (x1, x2)) (Range (y1, y2))
  | x2 < y1 || y2 < x1 = Nothing
  | otherwise = Just $ Range (max x1 y1, min x2 y2)

map :: (Integer -> Integer) -> Range -> Range
map f (Range (x1, x2)) = Range (f x1, f x2)

start :: Range -> Integer
start (Range (s, _)) = s

end :: Range -> Integer
end (Range (_, e)) = e
