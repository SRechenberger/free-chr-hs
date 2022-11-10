module CHR.Examples.Generic.List where

import CHR

data Coin = Unknown | Heads | Tails deriving (Show, Eq)

toss :: Solver solver => solver [] Coin
toss = simplify' "toss"
  [(== Unknown)]
  (const [[[Heads], [Tails]]])
