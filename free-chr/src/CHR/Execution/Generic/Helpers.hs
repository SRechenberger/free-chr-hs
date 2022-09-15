module CHR.Execution.Generic.Helpers where

import CHR.Execution.Generic

import Control.Monad.Except

type Head c = [c -> Bool]
type Guard m c = [c] -> m Bool
type Body m c = [c] -> [m [c]]

true :: Monad m => Guard m c
true = const $ pure True

false :: Monad m => Guard m c
false = const $ pure False

top :: Monad m => Body m c
top = const []

failWith :: MonadError e m => e -> Body m c
failWith e = const [throwError e]

bottom :: MonadError () m => Body m c
bottom = failWith ()

wildcard :: a -> Bool
wildcard = const True

rule' :: Monad m
  => String -> Head c -> Head c -> Body m c -> Solver m c
rule' n k r b = rule n k r true b

clean :: Monad m
  => String -> Head c -> Head c -> Guard m c -> Solver m c
clean n k r g = rule n k r g top

clean' :: Monad m
  => String -> Head c -> Head c -> Solver m c
clean' n k r = rule n k r true top

propagate :: Monad m
  => String -> Head c -> Guard m c -> Body m c -> Solver m c
propagate n h g b = rule n h [] g b

propagate' :: Monad m
  => String -> Head c -> Body m c -> Solver m c
propagate' n h b = rule' n h [] b

simplify :: Monad m
  => String -> Head c -> Guard m c -> Body m c -> Solver m c
simplify n h g b = rule n [] h g b

simplify' :: Monad m
  => String -> Head c -> Body m c -> Solver m c
simplify' n h b = rule' n [] h b

remove :: Monad m
  => String -> Head c -> Guard m c -> Solver m c
remove n h g = simplify n h g top

remove' :: Monad m
  => String -> Head c -> Solver m c
remove' n h = simplify' n h top
