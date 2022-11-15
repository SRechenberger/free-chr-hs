module CHR.Examples.FiniteDomain.WaveFunctionCollaps where

import Control.Monad.Random
import Control.Monad.Except
import Control.Applicative
import Control.Lens (at, (^.), to, (&))
import Control.Arrow

import GHC.Generics (Generic)

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Maybe (maybeToList, fromJust)

import CHR.FiniteDomain
import CHR.Helpers

import CHR.Examples.FiniteDomain.EnumConstraints (fd)


data Tile
  = Water | Grass | Forrest | Mountains
  deriving (Show, Eq, Ord, Enum, Generic)

instance Uniform Tile

type Point = (Int, Int)
type Constraint = FDConstraint Point Tile
type Solver = DefaultFDSolver Maybe Point Tile
type State = FDState Point Tile

neighbors :: Point -> [Point]
neighbors (x,y) = [(x+dx, y+dy) | (dx, dy) <- [(-1,0),(1,0),(0,-1),(0,1)]]


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
wfcSolver = (fd <.> neighborRules)

wfc' :: (MonadRandom m) 
     => State -> m (Maybe State)
wfc' s = foldl (<|>) Nothing <$> mapM action cs' 

  where
    action c@(p `InEnum` _) = do
      t <- uniform (domain c)
      pure $ run' wfcSolver [p `inEnum` [t]] s

    cs' = [ s^.hConstraints.at i | i <- [2..4] ]
      & map (map snd . concat . maybeToList . fmap Map.toList)
      & concat

wfc :: (MonadRandom m) => (Int, Int) -> m [Constraint]
wfc (w, h) = getConstraints . fromJust <$> wfc' s
    
  where
    s = fromJust $ run wfcSolver q
    q = [ p `inEnum` [Water .. Mountains] | p <- (,) <$> [0..w] <*> [0..h] ]
