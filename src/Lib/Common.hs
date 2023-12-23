module Lib.Common (nTimes, whileM) where

import Control.Monad (MonadPlus (mzero, mplus))

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = go n
  where
    go 0 = id
    go i = f . go (i - 1)

-- Stolen this definition from Control.Monad.Loops
whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM p f = go
 where
  go = do
    x <- p
    if x
      then do
        x' <- f
        xs <- go
        return (return x' `mplus` xs)
      else return mzero
