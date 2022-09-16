module CHR.Examples.Generic.Writer where

import CHR.Execution.Generic.Simple
import CHR.Execution.Generic.Helpers

import Control.Monad.Writer

fib' :: Solver (Writer [Integer]) (Integer, Integer)
fib' = simplify' "next"
  [wildcard]
  (\[(a, b)] ->
    [ (tell [a] >> pure [])
    , pure [(b, a + b)]])
