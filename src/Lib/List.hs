module Lib.List (safeHead, setAt, (!?), pairs, mapWithIndex, findCycle) where

import qualified Data.Map.Strict as M

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
             -- Definition adapted from GHC.List
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n


setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

pairs :: Eq a => [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "Odd length list provided to pairs"
pairs (x1:x2:xs) = (x1, x2) : pairs xs

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = go 0 
  where
    go _ [] = []
    go idx (x:xs) = f idx x : go (idx + 1) xs

findCycle :: Ord a => [a] -> Maybe (Int, Int)
findCycle = go M.empty 0
  where
    go _ _ [] = Nothing
    go seen i (x:xs) =
      case M.lookup x seen of
        Nothing -> go (M.insert x i seen) (i + 1) xs
        Just j  -> Just (j, i)
