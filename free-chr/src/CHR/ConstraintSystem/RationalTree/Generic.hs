module CHR.ConstraintSystem.RationalTree.Generic (
  RationalTree (..),
  EqConstraint (..)
) where

-- Class Defintions
class RationalTree a where
  type FunctionSymbol a :: *
  type Argument a :: *
  arity     :: a -> Int
  functor   :: a -> Maybe (FunctionSymbol a)
  arguments :: a -> [Argument a]


class RationalTree a => EqConstraint a where
  type Constraint a :: *
  eq :: a -> a -> Constraint a
