module CHR.FiniteDomainSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit.Lang (assertFailure)

import Test.QuickCheck (Gen, NonEmptyList (..), getPositive, getNonNegative, Arbitrary (..))

import Control.Monad.Random

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (fromJust)

import CHR.Helpers
import CHR.FiniteDomain
import CHR.Examples.FiniteDomain.EnumConstraints
import CHR.Examples.FiniteDomain.WaveFunctionCollapse


data Vars = A | B | C | D
  deriving (Eq, Ord, Show)


instance (Arbitrary s, Ord v, Arbitrary v) => Arbitrary (FDConstraint s v) where
  arbitrary = do
    i <- (arbitrary :: Gen Int)
    case i `mod` 2 of
      0 -> InEnum <$> arbitrary <*> arbitrary
      1 -> Eq <$> arbitrary <*> arbitrary

instance Arbitrary LandscapeTile where
  arbitrary = toEnum . (`mod` fromEnum (maxBound :: LandscapeTile)) <$> arbitrary

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

  describe "CHR.Examples.FiniteDomains.WaveFunctionCollapse" $ do
    prop "outOfbounds removes out of bounds constraints" $ \(ps :: NonEmptyList (FDConstraint Point LandscapeTile)) -> do
      let q = [IdentifierBounds (0, 0) (10, 10)] <> (getNonEmpty ps)
      fromJust (evaluate outOfBounds q)
        `shouldMatchList` ([IdentifierBounds (0, 0) (10, 10)] <> [c | c <- getNonEmpty ps, all (\(x, y) -> between 0 10 x && between 0 10 y) (identifiers c)])
    
    prop "wfc generates a valid grid" $ \seed -> do
      let dim = (10, 10)
      shouldSatisfy (evalRand (uncurry wfc dim) (mkStdGen seed)) $ maybe False $ (validGrid :: Grid LandscapeTile -> Bool)
