module CHR.Execution.Generic where

class Solver (solver :: (* -> *) -> * -> *) where
  type State solver :: * -> *

  rule :: (Functor m)
    => String -> [c -> Bool] -> [c -> Bool]
    -> ([c] -> m Bool) -> ([c] -> [m [c]])
    -> solver m c

  (<.>) :: solver m c -> solver m c -> solver m c
