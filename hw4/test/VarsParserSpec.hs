 module VarsParserSpec (main, spec) where

import           AExpr                 (AExpr (..))
import           AExprParser           ()
import           Test.Hspec
import           Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import           Text.Megaparsec       (parse, ParseError)
import           VarsParser            (Assign (..), assignParser)
import Data.Void (Void)
spec :: Spec
spec =
  it "assignParser" $ do
  test "mut y = x + 3 * (let x = 2 in x)" `shouldParse`
    Crt "y" (Var "x" `Sum` (Const 3 `Mul` ("x" `Loc` Const 2 $ Var "x")))
  test "y = y + 3 / (let ab = 12 * x in ab * 3)" `shouldParse`
    Rsgn "y" (Var "y" `Sum` (Const 3 `Div` ("ab" `Loc` (Const 12 `Mul` Var "x") $ Var "ab" `Mul` Const 3)))
  test `shouldFailOn` "mut2- 2"
  test `shouldFailOn` ","
  test `shouldFailOn` "let * 2"
  test `shouldFailOn` "+ 2"
  test "mut let3 = let2 * in3 - 3" `shouldParse`
    Crt "let3" ((Var "let2" `Mul` Var "in3") `Sub` Const 3)
  test "in2 = a ^ b ^ c ^ (a + b * c ^ d)" `shouldParse`
    Rsgn "in2" (Var "a" `Pow` (Var "b" `Pow` (Var "c" `Pow` (Var "a" `Sum` (Var "b" `Mul` (Var "c" `Pow` Var "d"))))))

main :: IO ()
main = hspec spec
------------------
test :: String -> Either (ParseError Char Data.Void.Void) Assign
test = parse assignParser ""
