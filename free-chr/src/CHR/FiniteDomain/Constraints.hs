module CHR.FiniteDomain.Constraints where

import Data.Set (Set)
import qualified Data.Set as Set

data FDConstraint s a
    = InEnum s (Set a)
    | Eq s s
  deriving (Show, Eq)

entropy :: FDConstraint s a -> Maybe Int
entropy (InEnum _ vs) = Just $ Set.size vs
entropy _             = Nothing

isInEnum :: FDConstraint s a -> Bool
isInEnum (InEnum _ _) = True
isInEnum _            = False

isEq :: FDConstraint s a -> Bool
isEq (Eq _ _) = True
isEq _        = False