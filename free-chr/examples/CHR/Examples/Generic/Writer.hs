module CHR.Examples.Generic.Writer where

import CHR

import Control.Monad.Writer

fib' :: Solver solver => solver (Writer [Integer]) (Integer, Integer)
fib' = simplify' "next"
  [wildcard]
  (\[(a, b)] ->
    [ (tell [a] >> pure [])
    , pure [(b, a + b)]])
