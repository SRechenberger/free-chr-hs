module CHR.Examples.FiniteDomain.WaveFunctionCollapse.Instances where

import Data.Aeson

import Control.Monad.Random (Uniform)

import GHC.Generics (Generic)

import CHR.Examples.FiniteDomain.WaveFunctionCollapse.Tile

data LandscapeTile
  = Water | Grass | Forrest | Mountains
  deriving (Show, Eq, Ord, Enum, Generic, Bounded)
    
instance Uniform LandscapeTile

instance Tile LandscapeTile where
  allowedNeighbors t = case t of
    Water     -> [Water, Grass]
    Grass     -> [Water, Grass, Forrest, Mountains]
    Forrest   -> [Grass, Forrest, Mountains]
    Mountains -> [Grass, Forrest, Mountains]

instance ToJSON LandscapeTile where
  toEncoding = genericToEncoding defaultOptions