module CHR.FiniteDomain.Default where

import CHR.FiniteDomain.Solver

import CHR.FiniteDomain.State
import CHR.FiniteDomain.Constraints

import Control.Arrow
import Control.Lens
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

-- import Data.Maybe (isJust)
-- import Data.List (subsequences, permutations)
import Data.Foldable.Extra (findM)
-- import Data.Bifunctor (bimap)

import Control.Applicative (liftA2)


newtype DefaultFDSolver m s v = FDSolver 
    { runFDSolver ::
        Int -> FDConstraint s v -> FDState s v
        -> m (Maybe ([m [FDConstraint s v]], FDState s v)) }


instance Monad m => Semigroup (DefaultFDSolver m s v) where
    f <> g = FDSolver { runFDSolver = solve }
      where
        solve i c s = do
            rf <- runFDSolver f i c s
            maybe (runFDSolver g i c s) (pure >>> pure) rf


instance FDSolver DefaultFDSolver where
    rule name kept removed guard body = FDSolver
        { runFDSolver = solver }
      where
        solver i c s = do
            matching <- findM
                (unzip
                >>> bimap (\is -> pure $ check name is s) guard
                >>> uncurry (liftA2 (&&)))
                $ match i c kept removed
                    (concat $ map Map.toList $ map snd $ Map.toList $ s^.hConstraints)
                    (Map.toList $ s^.constraints)
            pure $ do
                m <- matching
                let (is, cs) = unzip m
                let rs = drop (length kept) is
                let s' = s
                        & kills rs
                        & (if length rs > 0 then id else record name is)
                pure (body cs, s')

    (<.>) = (<>)


-- TODO
match :: Int -> FDConstraint s v
      -> [FDConstraint s v -> Bool] -> [FDConstraint s v -> Bool]
      -> [(Int, a)] -> [(Int, a)]
      -> [[(Int, a)]]
match i c ks rs hcs cs = error "Not yet implemented"


run :: (Monad m, Eq s, Eq v) => DefaultFDSolver m s v -> [FDConstraint s v] -> m (FDState s v)
run solver query = run' solver query newFDState


run' :: (Monad m, Eq s, Eq v) 
     => DefaultFDSolver m s v -> [FDConstraint s v] -> FDState s v -> m (FDState s v)
run' _ [] state = pure state
run' solver (c:cs) state = do
    let (i, state') = fresh state
    call solver i c (add i c state') >>= run' solver cs


call :: (Monad m, Eq s, Eq v)
     => DefaultFDSolver m s v -> Int -> FDConstraint s v -> FDState s v -> m (FDState s v)
call solver i c state
  | isAlive i state = do
    mr <- runFDSolver solver i c state
    case mr of
      Nothing            -> pure state
      Just (mqs, state') -> do
        state'' <- foldM
          (\s mq -> join $ run' solver <$> mq <*> pure s)
          state'
          mqs
        (if isAlive i state'' then call solver i c else pure) state''


evaluate :: (Monad m, Eq s, Eq v)
         => DefaultFDSolver m s v -> [FDConstraint s v] -> m [FDConstraint s v]
evaluate solver = run solver
  >=> (_hConstraints &&& _constraints)
  >>> (concat . map (Map.toList . snd) . Map.toList) `bimap` Map.toList
  >>> uncurry (<>)
  >>> unzip >>> snd >>> pure