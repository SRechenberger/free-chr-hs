{-# LANGUAGE TemplateHaskell #-}

module CHR.FiniteDomain.State where

import CHR.FiniteDomain.Constraints

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Lens
import Control.Arrow
import Data.Bifunctor (bimap)


data FDState s v = FDState
  { _nextId       :: Int
  , _hConstraints :: Map Int (Map Int (FDConstraint s v))
  , _constraints  :: Map Int (FDConstraint s v)
  , _entropyOf    :: Map Int Int
  , _alive        :: Set Int
  , _history      :: Set (String, [Int])
  }
  deriving (Show)

makeLenses ''FDState


newFDState :: Eq s => FDState s c
newFDState = FDState
  { _nextId       = 0
  , _hConstraints = Map.empty
  , _constraints  = Map.empty
  , _entropyOf    = Map.empty
  , _alive        = Set.empty
  , _history      = Set.empty
  }


getConstraints :: FDState s v -> [FDConstraint s v]
getConstraints =
  _hConstraints &&& _constraints
  >>> (concat . map (Map.toList . snd) . Map.toList) `bimap` Map.toList
  >>> uncurry (<>)
  >>> unzip >>> snd


fresh :: FDState s c -> (Int, FDState s c)
fresh = id &&& id
  >>> first (^. nextId)
  >>> (\(i, s) -> (i, s & alive %~ Set.insert i))
  >>> second (nextId +~ 1)


add :: (Eq s, Eq v) => Int -> FDConstraint s v -> FDState s v -> FDState s v
add i c = case entropy c of
    Nothing -> constraints . at i ?~ c
    Just h  -> hConstraints . at h . non Map.empty . at i ?~ c
        >>> entropyOf . at i ?~ h 


kill :: Int -> FDState s v -> FDState s v
kill i s =  s
    & alive %~ Set.delete i
    & del
  where
    del = case s^.entropyOf . at i of
        Nothing -> constraints %~ Map.delete i
        Just h  -> hConstraints . at h %~ fmap (Map.delete i)


isAlive :: Int -> FDState s v -> Bool
isAlive i = (^. alive . to (elem i))


kills :: [Int] -> FDState s v -> FDState s v
kills is s = foldr kill s is


record :: String -> [Int] -> FDState s v -> FDState s v
record r is = history %~ Set.insert (r, is)


check :: String -> [Int] -> FDState s c -> Bool
check r is = (^. history . to (Set.member (r, is))) >>> not
