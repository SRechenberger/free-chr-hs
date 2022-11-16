module CHR.FiniteDomain.Constraints where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (nub)

data FDConstraint s a
    = InEnum s (Set a)
    | Eq s s
    | IdentifierBounds s s
  deriving (Show, Eq, Ord)

entropy :: FDConstraint s a -> Maybe Int
entropy (InEnum _ vs) = Just $ Set.size vs
entropy _             = Nothing

isInEnum :: FDConstraint s a -> Bool
isInEnum (InEnum _ _) = True
isInEnum _            = False

isEq :: FDConstraint s a -> Bool
isEq (Eq _ _) = True
isEq _        = False

isIdentifierBounds :: FDConstraint s a -> Bool
isIdentifierBounds (IdentifierBounds _ _) = True
isIdentifierBounds _                      = False

inEnum :: (Eq s, Ord v) => s -> [v] -> FDConstraint s v
inEnum s vs = s `InEnum` Set.fromList vs

identifiers :: Eq s => FDConstraint s v -> [s]
identifiers (InEnum s _) = [s]
identifiers (Eq a b)     = nub [a, b]

domain :: FDConstraint s v -> [v]
domain (InEnum s vs) = Set.toList vs
domain _             = error "No Domain Constraint"

hasDomain :: Eq v => [v] -> FDConstraint s v -> Bool
hasDomain vs = (vs ==) . domain