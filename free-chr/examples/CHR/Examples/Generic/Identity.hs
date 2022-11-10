module CHR.Examples.Generic.Identity where

import CHR

import Data.Functor.Identity

gcd' :: Solver solver => solver Identity Int
gcd' =
  remove' "zero" [(<= 0)] <.>
  rule "subtract" [(> 0)] [(> 0)]
    (\[n, m] -> pure $ n <= m)
    (\[n, m] -> [pure $ [m - n]])

nub' :: (Solver solver, Eq a) => solver Identity a
nub' = clean "remove duplicate" [wildcard] [wildcard]
  (\[x, y] -> pure $ x == y)
