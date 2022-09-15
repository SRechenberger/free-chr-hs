import Test.Hspec

import qualified CHR.Execution.GenericSpec as GenericExecution

main :: IO ()
main = hspec $ do
  GenericExecution.spec
