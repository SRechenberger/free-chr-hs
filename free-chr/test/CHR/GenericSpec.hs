{-# LANGUAGE ScopedTypeVariables #-}

module CHR.GenericSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck (NonEmptyList (..), getPositive, getNonNegative, verbose)

import CHR

import CHR.Examples.Generic.Identity
import CHR.Examples.Generic.Writer
import CHR.Examples.Generic.List
import CHR.Examples.Generic.FiniteDomain

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (fromJust)

import Control.Lens ((^.), (&))
import Control.Monad.Writer
import Control.Monad (when)

skip :: Monad m => String -> m () -> m ()
skip _ = when False

data Vars = A | B | C | D
  deriving (Eq, Ord, Show)

spec :: Spec
spec = do
  describe "CHR.Generic.Simple.match" $ do
    it "finds a valid matching" $ do
      let s = [(0, 120), (1, 36), (2, 12)]
      let h = [(> 0), (> 0)]
      match 0 120 h s `shouldBe`
        [ [(0, 120), (1, 36)], [(1, 36), (0, 120)]
        , [(0, 120), (2, 12)], [(2, 12), (0, 120)]]

  describe "CHR.Generic.Identity.gcd'" $
    prop "correctly computes the gcd" $ \(k, q') ->
      let
        q = map (getPositive k *) $ map getPositive $ getNonEmpty q'
        s = evaluate gcd' q & runIdentity
      in s `shouldBe` [foldr1 gcd q]

  describe "CHR.Generic.Identity.nub'" $
    modifyMaxSize (const 20) $
      prop "removes any duplicates" $ \(q' :: NonEmptyList Int) ->
        let
          q = getNonEmpty q'
          s = evaluate nub' q & runIdentity
        in s `shouldBe` nub q

  describe "CHR.Generic.Writer.fib" $
    prop "lazily computes the fibonacci sequence" $ \(a', b', n') ->
      let
        a = getPositive a'
        b = getPositive b'
        n = getNonNegative n'
        s = run fib' [(min a b, max a b)] & execWriter
      in take n s `shouldBe` take n (fib (min a b) (max a b))

  describe "CHR.Generic.List.toss" $
    it "simulates non-determinism" $ do
      evaluate toss [] `shouldMatchList` [[]]
      evaluate toss [Unknown] `shouldMatchList` [[Heads], [Tails]]
      evaluate toss [Unknown, Unknown] `shouldMatchList`
        (\a b -> [a, b]) <$> [Heads, Tails] <*> [Heads, Tails]

  describe "CHR.Generic.FiniteDomains" $ do
    it "handles inconsistency correctly" $ do
      evaluate fd [A `In` []]
        `shouldBe` (Nothing :: Maybe [EnumConstraint Vars Int])

    it "handles intersections correctly" $ do
      evaluate fd [A `In` [1,2,3], A `In` [2,3,4]]
        `shouldBe` Just [A `In` [2,3]]
      evaluate fd [A `In` [1,2,3], A `In` [4]]
        `shouldBe` (Nothing :: Maybe [EnumConstraint Vars Int])

    it "handles intersections trigged by `Eq` correctly" $ do
      fromJust (evaluate fd [A `In` [1,2,3], B `In` [2,3,4], A `Eq` B])
        `shouldMatchList` [A `In` [2,3], B `In` [2,3], A `Eq` B]
      fromJust (evaluate fd [A `In` [1,2,3], B `In` [2,3,4]])
        `shouldMatchList` [A `In` [1,2,3], B `In` [2,3,4]]
      evaluate fd [A `In` [1,2,3], B `In` [4], A `Eq` B]
        `shouldBe` (Nothing :: Maybe [EnumConstraint Vars Int])

    skip "won't terminate" $ it "handles leq constraints correctly" $ do
      fromJust (evaluate (eq <.> leq) [Leq A B, Leq B A])
        `shouldMatchList` ([Eq A B, Eq B A] :: [EnumConstraint Vars Int])

    skip "won't terminate" $ it "handles eq constraints correctly" $ do
      fromJust (evaluate eq [Eq A B])
        `shouldMatchList` ([Eq A B, Eq B A] :: [EnumConstraint Vars Int])




fib :: Integer -> Integer -> [Integer]
fib a b = a : fib b (a + b)
