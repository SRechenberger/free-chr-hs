module CHR.Examples.FiniteDomain.EnumConstraints where

import CHR.FiniteDomain

import qualified Data.Set as Set
import Control.Monad.Except
import Control.Applicative (liftA2)

fd :: (Eq s, Ord c, FDSolver solver, MonadError () m)
  => solver m s c
fd =
  inconsistent' "inconsistency" [liftA2 (&&) isInEnum (\(_ `InEnum` d) -> Set.null d)] <.>

  simplify "intersection" [isInEnum, isInEnum]
    (\[x `InEnum` _, y `InEnum` _] -> pure $ x == y)
    (\[x `InEnum` d1, _ `InEnum` d2] -> [pure [x `InEnum` Set.intersection d1 d2]]) <.>

  rule "intersection'" [] [isInEnum, isInEnum]
    (\[x `InEnum` dx, y `InEnum` dy] ->
      pure $ x == y && dx /= dy)
    (\[x  `InEnum` dx, y  `InEnum` dy] ->
      [pure
        [ x `InEnum` Set.intersection dx dy
        , y `InEnum` Set.intersection dx dy
        ]])