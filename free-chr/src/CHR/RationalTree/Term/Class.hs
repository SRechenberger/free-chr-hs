module CHR.RationalTree.Term.Class (
  RationalTree (..)
) where

class RationalTree a where
  type FunctionSymbol a :: *
  type Argument a :: *
  arity     :: a -> Int
  functor   :: a -> Maybe (FunctionSymbol a)
  arguments :: a -> [Argument a]
