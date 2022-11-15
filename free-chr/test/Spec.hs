import Test.Hspec

import qualified CHR.GenericSpec as Generic
import qualified CHR.FiniteDomainSpec as FiniteDomain

main :: IO ()
main = hspec $ do
  Generic.spec
  FiniteDomain.spec
