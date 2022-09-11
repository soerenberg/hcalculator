import Lib.Internal
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ evalTests
  , parseTests
  , endToEndTests
  ]

evalTests :: TestTree
evalTests = testGroup "EvalTests"
  [ testCase "eval 3+4" $ (eval (Add (Atom 3) (Atom 4))) @?= (Right 7)
  , testCase "eval 7-3" $ (eval (Sub (Atom 7) (Atom 3))) @?= (Right 4)
  , testCase "eval 3*2" $ (eval (Mul (Atom 3) (Atom 2))) @?= (Right 6)
  , testCase "eval 10/4" $ (eval (Div (Atom 10) (Atom 4))) @?= (Right 2.5)
  , testCase "eval (3+2)*4" $ (eval $ Mul (Add (Atom 3) (Atom 2)) (Atom 4)) @?=
                              (Right 20)
  ]

parseTests :: TestTree
parseTests = testGroup "ParseTests"
  [ testCase "parse '2'" $ parse parseNumber "hcalc" "2" @?= (Right . Atom $ 2)
  , testCase "parse '-3 '" $
      parse parseNumber "hcalc" "-3" @?= (Right . Atom $ -3)
  , testCase "parse '+79'" $
      parse parseNumber "hcalc" "+79" @?= (Right . Atom $ 79)
  , testCase "parse '4 '" $
      parse parseNumber "hcalc" "4 " @?= (Right . Atom $ 4)
  , testCase "parse '3.141'" $
      parse parseNumber "hcalc" "3.141" @?= (Right . Atom $ 3.141)
  ]

endToEndTests :: TestTree
endToEndTests = testGroup "EndToEnd"
  [ testCase "endToEnd '2 * 4'" $ (evaledToStr $ (readExpr "2 * 4") >>= eval)
      @?= "8.0"
  , testCase "endToEnd '2 + 4 * 2'" $
      (evaledToStr $ (readExpr "2 + 4 * 2") >>= eval) @?= "10.0"
  , testCase "endToEnd '(2 + 4) * 2'" $
      (evaledToStr $ (readExpr "(2 + 4) * 2") >>= eval) @?= "12.0"
  ]
