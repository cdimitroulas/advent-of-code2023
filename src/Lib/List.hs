module Lib.List (safeHead) where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
