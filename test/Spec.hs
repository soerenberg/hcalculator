import Lib.Internal
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testExtractRight
  ]

testExtractRight = testCase "extractRight" $ (extractRight $ Right "a") @?= "a"
