module Lib.Common (nTimes) where

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = go n
  where
    go 0 = id
    go i = f . go (i - 1)
