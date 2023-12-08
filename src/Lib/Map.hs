module Lib.Map where
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

getCounts :: Ord a => [a] -> Map a Int
getCounts = foldl' (flip (M.alter (\val -> Just $ fromMaybe 0 val + 1))) M.empty
