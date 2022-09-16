module CHR.Examples.Generic.List where

import CHR.Execution.Generic.Simple
import CHR.Execution.Generic.Helpers

data Coin = Unknown | Heads | Tails deriving (Show, Eq)

toss :: Solver [] Coin
toss = simplify' "toss"
  [(== Unknown)]
  (const [[[Heads], [Tails]]])
