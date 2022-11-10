module CHR.FiniteDomainSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonEmptyList (..), getPositive, getNonNegative, verbose)

import qualified Data.Set as Set

import CHR.FiniteDomain
import CHR.Examples.FiniteDomain.EnumConstraints

data Vars = A | B | C | D
  deriving (Eq, Ord, Show)

spec = do
  describe "CHR.Execution.Generic.FiniteDomains" $ do
    it "handles inconsistency correctly" $ do
      evaluate fd [A `InEnum` Set.empty]
        `shouldBe` (Nothing :: Maybe [FDConstraint Vars Int])

    it "handles intersections correctly" $ do
      evaluate fd [A `InEnum` Set.fromList [1,2,3], A `InEnum` Set.fromList [2,3,4]]
        `shouldBe` Just [A `InEnum` Set.fromList [2,3]]
      evaluate fd [A `InEnum` Set.fromList [1,2,3], A `InEnum` Set.fromList [4]]
        `shouldBe` (Nothing :: Maybe [FDConstraint Vars Int])
