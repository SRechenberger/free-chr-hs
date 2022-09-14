module CHR.ConstraintSystem.RationalTree.Default where

import CHR.ConstraintSystem.RationalTree.Generic

data Term c v
  = Constant c
  | Variable v
  | Compound String [Term c v]
  deriving(Show, Eq)


instance RationalTree (Term c v) where
  type FunctionSymbol (Term c v) = (String, Int)
  type Argument (Term c v) = Term c v

  arity (Compound _ as) = length as
  arity _ = 0

  functor (Compound s as) = Just (s, length as)
  functor _ = Nothing

  arguments (Compound _ as) = as
  arguments _ = []


data Equal c v =
  Term c v `Eq` Term c v

instance EqConstraint (Term c v) where
  type Constraint (Term c v) = Equal c v
  eq = Eq
