module CHR.Examples.Generic.GCD where

import CHR.Execution.Generic

import Data.Functor.Identity

gcd' :: Solver Identity Int
gcd' =
  rule "zero" [] [(<= 0)] (const (pure True)) (const []) <.>
  rule "subtract" [(> 0)] [(> 0)]
    (\[n, m] -> pure $ n <= m)
    (\[n, m] -> [pure $ [m - n]])
