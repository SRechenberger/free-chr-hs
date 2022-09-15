module CHR.Examples.Generic.Identity where

import CHR.Execution.Generic
import CHR.Execution.Generic.Helpers

import Data.Functor.Identity

gcd' :: Solver Identity Int
gcd' =
  remove' "zero" [(<= 0)] <.>
  rule "subtract" [(> 0)] [(> 0)]
    (\[n, m] -> pure $ n <= m)
    (\[n, m] -> [pure $ [m - n]])

nub' :: Eq a => Solver Identity a
nub' = clean "remove duplicate" [wildcard] [wildcard]
  (\[x, y] -> pure $ x == y)
