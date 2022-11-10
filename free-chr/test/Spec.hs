import Test.Hspec

import qualified CHR.GenericSpec as Generic

main :: IO ()
main = hspec $ do
  Generic.spec
