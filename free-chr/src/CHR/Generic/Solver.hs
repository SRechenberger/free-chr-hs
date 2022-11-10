module CHR.Generic.Solver where

class Solver (solver :: (* -> *) -> * -> *) where
  rule :: (Monad m)
    => String -> [c -> Bool] -> [c -> Bool]
    -> ([c] -> m Bool) -> ([c] -> [m [c]])
    -> solver m c

  (<.>) :: (Monad m) => solver m c -> solver m c -> solver m c
