module CHR.Execution.GenericSpec where

import Test.Hspec

import CHR.Execution.Generic
import CHR.Examples.Generic.GCD

import Data.Functor.Identity
import qualified Data.Map as Map

import Control.Lens ((^.), (&))


spec :: Spec
spec = do
  describe "CHR.Execution.Generic.match" $ do
    it "finds a valid matching" $ do
      let s = [(0, 120), (1, 36), (2, 12)]
      let h = [(> 0), (> 0)]
      match 0 120 h s `shouldBe`
        [ [(0, 120), (1, 36)], [(1, 36), (0, 120)]
        , [(0, 120), (2, 12)], [(2, 12), (0, 120)]]

  describe "CHR.Execution.Generic" $ do
    it "correctly computes the gcd" $ do
      let s = run gcd' [120, 36]
              & runIdentity
              & (^. constraints)
              & Map.toList
              & unzip
              & snd
      s `shouldBe` [gcd 120 36]
