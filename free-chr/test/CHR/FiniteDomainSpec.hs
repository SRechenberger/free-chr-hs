module CHR.FiniteDomainSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonEmptyList (..), getPositive, getNonNegative, verbose)
import Test.HUnit.Lang (assertFailure)

import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (fromJust)

import CHR.FiniteDomain
import CHR.Examples.FiniteDomain.EnumConstraints


data Vars = A | B | C | D
  deriving (Eq, Ord, Show)

spec = do
  describe "CHR.FiniteDomains.match" $ do
    it "does not match the called constraint again" $ do
      let c = A `InEnum` Set.fromList [()]
      match 0 c
        [const True, const True] []
        [(0, c)] []
        `shouldBe` []
      
  describe "CHR.FiniteDomains.EnumConstraints" $ do
    it "handles inconsistency correctly" $ do
      evaluate fd [A `InEnum` Set.empty]
        `shouldBe` (Nothing :: Maybe [FDConstraint Vars Int])

    it "handles subsumption correctly" $ do
      evaluate fd [A `InEnum` Set.fromList [1,2,3], A `InEnum` Set.fromList [1,2]]
        `shouldBe` Just [A `InEnum` Set.fromList [1,2]]

    it "handles intersections correctly" $ do
      evaluate fd [A `InEnum` Set.fromList [1,2,3], A `InEnum` Set.fromList [2,3,4]]
        `shouldBe` Just [A `InEnum` Set.fromList [2,3]]

      evaluate fd [A `InEnum` Set.fromList [1,2,3], A `InEnum` Set.fromList [4]]
        `shouldBe` (Nothing :: Maybe [FDConstraint Vars Int])

      
    it "handles intersections trigged by `Eq` correctly" $ do
      let solver = fd <.> eq <.> fd_eq
      fromJust (evaluate solver [A `inEnum` [1,2,3], B `inEnum` [2,3,4], A `Eq` B])
        `shouldMatchList` [A `inEnum` [2,3], B `inEnum` [2,3], A `Eq` B, B `Eq` A]

      fromJust (evaluate solver [A `inEnum` [1,2,3], B `inEnum` [2,3,4]])
        `shouldMatchList` [A `inEnum` [1,2,3], B `inEnum` [2,3,4]]

      evaluate solver [A `inEnum` [1,2,3], B `inEnum` [4], A `Eq` B]
        `shouldBe` (Nothing :: Maybe [FDConstraint Vars Int])

    it "handles eq idempotency correctly" $ do
      case evaluate eq [A `Eq` A, A `Eq` B, B `Eq` B :: FDConstraint Vars ()] of
        Nothing -> assertFailure "Got Nothing"
        Just s  -> s `shouldMatchList` [A `Eq` B, B `Eq` A]

    it "handles eq reflexivity correctly" $ do
      evaluate eq [A `Eq` A :: FDConstraint Vars ()]
        `shouldBe` Just []
    
    it "handles eq symmetry correctly" $ do
      evaluate eq [A `Eq` B :: FDConstraint Vars ()]
        `shouldBe` Just [A `Eq` B, B `Eq` A]
      
    it "handles eq transitivity correctly" $ do
      case evaluate eq [A `Eq` B, B `Eq` C :: FDConstraint Vars ()] of
        Nothing -> assertFailure "Got Nothing"
        Just r  -> r `shouldMatchList`
          [ A `Eq` B, B `Eq` C, A `Eq` C
          , B `Eq` A, C `Eq` B, C `Eq` A ]
