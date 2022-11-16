module CHR.Examples.FiniteDomain.WaveFunctionCollaps where

import Control.Monad.Random
import Control.Monad.Except
import Control.Applicative
import Control.Lens (at, (^.), to, (&), (?~))
import Control.Arrow

import GHC.Generics (Generic)

import Data.Function
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (toList)
import Data.Maybe (maybeToList, fromJust, catMaybes)

import CHR.FiniteDomain
import CHR.Helpers

import CHR.Examples.FiniteDomain.EnumConstraints (fd)

import System.Random.Shuffle

import Debug.Trace


data Tile
  = Water | Grass | Forrest | Mountains
  deriving (Show, Eq, Ord, Enum, Generic, Bounded)

instance Uniform Tile

type Point = (Int, Int)
type Constraint = FDConstraint Point Tile
type Solver = DefaultFDSolver Maybe Point Tile
type State = FDState Point Tile

neighbors :: Point -> [Point]
neighbors (x,y) = [(x+dx, y+dy) | (dx, dy) <- [(-1,0),(1,0),(0,-1),(0,1)]]

outOfBounds :: Solver
outOfBounds = clean "out of bounds"
  [isIdentifierBounds] [wildcard]
  (\[IdentifierBounds (lx, ly) (ux, uy), c] -> pure $ any (\(x, y) -> not $ between lx ux x && between ly uy y) (identifiers c))

neighborRule :: Tile -> [Tile] -> Solver
neighborRule tile nTiles = propagate'
  (show tile) [isInEnum &&. hasDomain [tile]]
    (\[p `InEnum` _] -> [pure [p' `inEnum` nTiles | p' <- neighbors p]])

neighborRules :: Solver
neighborRules =
  neighborRule Water     [Water, Grass] <.>
  neighborRule Forrest   [Forrest, Grass, Mountains] <.>
  neighborRule Mountains [Mountains, Grass, Forrest]

wfcSolver :: Solver 
wfcSolver = (outOfBounds <.> fd <.> neighborRules)


undetermined :: State -> [Constraint]
undetermined s = map snd $ concat $ catMaybes [Map.toList <$> (s^.hConstraints.at h) | h <- [2..4]]


wfc :: (MonadRandom m) => Int -> Int -> m (Maybe (Map Point [Tile]))
wfc w h = case init of 
    Nothing -> pure Nothing
    Just s  -> do
      sFinal <- wfc' (undetermined s) s
      pure $ (constraintsToGrid . filter isInEnum . getConstraints) <$> sFinal

  
  where
    query = IdentifierBounds (0,0) (w,h) : [ p `inEnum` [minBound..maxBound] | p <- (,) <$> [0..w] <*> [0..h] ]
    init = run wfcSolver query


wfc' :: (MonadRandom m) => [Constraint] -> State -> m (Maybe State)
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


constraintsToGrid :: [Constraint] -> Map Point [Tile]
constraintsToGrid = foldr save Map.empty
  where
    save c@(InEnum p _) = at p ?~ domain c
