module CHR.Examples.FiniteDomain.WaveFunctionCollapse.Generator where

import Control.Monad.Random
import Control.Monad.Except
import Control.Applicative
import Control.Lens (at, (^.), to, (&), (?~))
import Control.Arrow

import Data.Function
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (toList)
import Data.Maybe (maybeToList, fromJust, catMaybes)

import CHR.FiniteDomain
import CHR.Helpers

import CHR.Examples.FiniteDomain.EnumConstraints (fd)
import CHR.Examples.FiniteDomain.WaveFunctionCollapse.Tile

import System.Random.Shuffle

import Debug.Trace


type Point = (Int, Int)
type Constraint t = FDConstraint Point t
type Solver t = DefaultFDSolver Maybe Point t
type State t = FDState Point t
type Grid t = Map Point [t]


validGrid :: Tile t => Grid t -> Bool
validGrid grid = all
  (\(p, d) -> 
    length d == 1
    && all (\q -> maybe True (\tn -> allowed d tn) (Map.lookup q grid)) (neighbors p))
  (Map.toList grid)


neighbors :: Point -> [Point]
neighbors (x,y) = [(x+dx, y+dy) | (dx, dy) <- [(-1,0),(1,0),(0,-1),(0,1)]]


outOfBounds :: Tile t => Solver t
outOfBounds = clean "out of bounds"
  [isIdentifierBounds] [wildcard]
  (\[IdentifierBounds (lx, ly) (ux, uy), c] -> pure $ any (\(x, y) -> not $ between lx ux x && between ly uy y) (identifiers c))


neighborRule :: Tile t => t -> [t] -> Solver t
neighborRule tile nTiles = propagate'
  (show tile) [isInEnum &&. hasDomain [tile]]
    (\[p `InEnum` _] -> [pure [p' `inEnum` nTiles | p' <- neighbors p]])


neighborRules :: Tile t => Solver t
neighborRules = foldr1 (<.>) [neighborRule t (allowedNeighbors t) | t <- [minBound..maxBound]]


wfcSolver :: Tile t => Solver t
wfcSolver = (outOfBounds <.> fd <.> neighborRules)


undetermined :: Tile t => State t -> [Constraint t]
undetermined s = map snd $ concat $ catMaybes [Map.toList <$> (s^.hConstraints.at h) | h <- [2..m]]
  where
    m = maximum [ k | (k, _) <- Map.toList (s^.hConstraints)]  


wfc :: (MonadRandom m, Tile t) => Int -> Int -> m (Maybe (Map Point [t]))
wfc w h = case init of 
    Nothing -> pure Nothing
    Just s  -> do
      sFinal <- wfc' (undetermined s) s
      pure $ (constraintsToGrid . filter isInEnum . getConstraints) <$> sFinal
  where
    query = IdentifierBounds (0,0) (w,h) : [ p `inEnum` [minBound..maxBound] | p <- (,) <$> [0..w] <*> [0..h] ]
    init = run wfcSolver query


wfc' :: (MonadRandom m, Tile t) => [Constraint t] -> State t -> m (Maybe (State t))
wfc' []                      s = pure $ Just s
wfc' (c@(p `InEnum` _):cs) s = do
    ts <- shuffleM (domain c)
    try ts
  where
    try []     = pure Nothing
    try (t:ts) = do
      case run' wfcSolver [p `inEnum` [t]] s of
        Nothing -> try ts
        Just s' -> wfc' (undetermined s') s'


constraintsToGrid :: Tile t => [Constraint t] -> Grid t
constraintsToGrid = foldr save Map.empty
  where
    save c@(InEnum p _) = at p ?~ domain c
