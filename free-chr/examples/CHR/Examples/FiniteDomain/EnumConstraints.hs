module CHR.Examples.FiniteDomain.EnumConstraints where

import CHR.FiniteDomain

import qualified Data.Set as Set
import Control.Monad.Except
import Control.Applicative (liftA2)

fd, eq, fd_eq :: (Eq s, Ord c, FDSolver solver, MonadError () m)
  => solver m s c
fd =
  inconsistent' "inconsistency"
    [liftA2 (&&) isInEnum (\(_ `InEnum` d) -> Set.null d)] <.>

  rule "subsumption" 
    [isInEnum] [isInEnum]
    (\[x `InEnum` dx, y `InEnum` dy] -> pure $ x == y && Set.isSubsetOf dx dy)
    top <.>

  rule "intersection'" [] [isInEnum, isInEnum]
    (\[x `InEnum` dx, y `InEnum` dy] ->
      pure $ x == y && dx /= dy)
    (\[x  `InEnum` dx, y  `InEnum` dy] ->
      [pure [ x `InEnum` Set.intersection dx dy ]])


eq =
  clean "idempotency" [isEq] [isEq]
    (\[x `Eq` y, x' `Eq` y'] -> pure $ x == x' && y == y') <.>

  remove "reflexivity" [isEq]
    (\[x `Eq` x'] -> pure $ x == x') <.>
  
  propagate "symmetry" [isEq]
    (\[x `Eq` y] -> pure $ x /= y)
    (\[x `Eq` y] -> [pure [y `Eq` x]]) <.>

  propagate "transitivity" [isEq, isEq]
    (\[x `Eq` y, y' `Eq` z] -> pure $ y == y')
    (\[x `Eq` y, y' `Eq` z] -> [pure [x `Eq` z]])


fd_eq =
  rule "subsumption'" [isEq, isInEnum] [isInEnum]
    (\[x' `Eq` y', x `InEnum` dx, y `InEnum` dy] ->
        pure $ x' == x && y' == y && dx /= dy && Set.isSubsetOf dx dy)
    (\[_ , x `InEnum` dx, y `InEnum` _] -> [pure [y `InEnum` dx]]) <.>

  rule "intersection'" [isEq] [isInEnum, isInEnum]
    (\[x' `Eq` y', x `InEnum` dx, y `InEnum` dy] ->
      pure $ x' == x && y' == y && dx /= dy)
    (\[_ , x `InEnum` dx, y `InEnum` dy] ->
      [pure $ let d = Set.intersection dx dy in [InEnum x d, InEnum y d]])

