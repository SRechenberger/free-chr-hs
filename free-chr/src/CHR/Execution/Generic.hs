{-# LANGUAGE TemplateHaskell #-}

module CHR.Execution.Generic where

import Data.Maybe (isJust)
import Data.List (subsequences, permutations)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.Except

import Control.Lens

import Control.Arrow

data CHRState c = CHRState
  { _nextId      :: Int
  , _constraints :: Map Int c
  , _alive       :: Set Int
  , _history     :: Set (String, [Int])
  }
  deriving (Show)

makeLenses ''CHRState

newtype Solver m c = Solver
  { runSolver :: Int -> c -> CHRState c -> m (Maybe ([m [c]], CHRState c)) }


fresh :: CHRState c -> (Int, CHRState c)
fresh = id &&& id
  >>> first (^. nextId)
  >>> (\(i, s) -> (i, s & alive %~ Set.insert i))
  >>> second (nextId +~ 1)


add :: Int -> c -> CHRState c -> CHRState c
add i c = constraints . at i ?~ c


kill :: Int -> CHRState c -> CHRState c
kill i = alive %~ Set.delete i
  >>> constraints %~ Map.delete i


record :: String -> [Int] -> CHRState c -> CHRState c
record r is = history %~ Set.insert (r, is)


check :: String -> [Int] -> CHRState c -> Bool
check r is = (^. history . to (Set.member (r, is)))


match :: Int -> a -> [a -> Bool] -> [(Int, a)] -> [[(Int, a)]]
match i c ps as = [ as'' |
    as' <- subsequences as,
    length as' == length ps,
    as'' <- permutations as',
    and (zipWith ($) ps (map snd as'')),
    i `elem` (map fst as'') ]


rule :: (Monad m)
     => [c -> Bool] -> [c -> Bool]
     -> ([c] -> m Bool)
     -> ([c] -> [m [c]])
     -> Solver m c
rule kept removed guard body = Solver { runSolver = solver }
  where
    solver i c state = do
      matchings <- filterM (map snd >>> guard)
        $ match i c (kept <> removed) (Map.toList $ state^.constraints)
      case matchings of
        []  -> pure Nothing
        cs:_ -> undefined
