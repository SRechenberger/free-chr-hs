{-# LANGUAGE ScopedTypeVariables #-}

module CHR.Execution.GenericSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck (NonEmptyList (..), getPositive, getNonNegative, verbose)

import CHR.Execution.Generic.Simple
import CHR.Examples.Generic.Identity
import CHR.Examples.Generic.Writer
import CHR.Examples.Generic.List

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.List (nub)

import Control.Lens ((^.), (&))
import Control.Monad.Writer


spec :: Spec
spec = do
  describe "CHR.Execution.Generic.Simple.match" $ do
    it "finds a valid matching" $ do
      let s = [(0, 120), (1, 36), (2, 12)]
      let h = [(> 0), (> 0)]
      match 0 120 h s `shouldBe`
        [ [(0, 120), (1, 36)], [(1, 36), (0, 120)]
        , [(0, 120), (2, 12)], [(2, 12), (0, 120)]]

  describe "CHR.Execution.Generic.Identity.gcd'" $
    prop "correctly computes the gcd" $ \(k, q') ->
      let
        q = map (getPositive k *) $ map getPositive $ getNonEmpty q'
        s = evaluate gcd' q & runIdentity
      in s `shouldBe` [foldr1 gcd q]

  describe "CHR.Execution.Generic.Identity.nub'" $
    modifyMaxSize (const 20) $
      prop "removes any duplicates" $ \(q' :: NonEmptyList Int) ->
        let
          q = getNonEmpty q'
          s = evaluate nub' q & runIdentity
        in s `shouldBe` nub q

  describe "CHR.Execution.Generic.Writer.fib" $
    prop "lazily computes the fibonacci sequence" $ \(a', b', n') ->
      let
        a = getPositive a'
        b = getPositive b'
        n = getNonNegative n'
        s = run fib' [(min a b, max a b)] & execWriter
      in take n s `shouldBe` take n (fib (min a b) (max a b))

  describe "CHR.Execution.Generic.List.toss" $
    it "simulates non-determinism" $ do
      evaluate toss [] `shouldMatchList` [[]]
      evaluate toss [Unknown] `shouldMatchList` [[Heads], [Tails]]
      evaluate toss [Unknown, Unknown] `shouldMatchList`
        (\a b -> [a, b]) <$> [Heads, Tails] <*> [Heads, Tails]


fib :: Integer -> Integer -> [Integer]
fib a b = a : fib b (a + b)
