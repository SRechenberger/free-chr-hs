module CHR.Examples.FiniteDomain.WaveFunctionCollapse.Tile where

import Control.Monad.Random (Uniform)

class (Uniform tile, Show tile, Eq tile, Ord tile, Enum tile, Bounded tile) => Tile tile where
  allowedNeighbors :: tile -> [tile]


allowed :: Tile t => [t] -> [t] -> Bool
allowed ts ts' = and $ allowed' <$> ts <*> ts'
  where
    allowed' t t' = t' `elem` allowedNeighbors t