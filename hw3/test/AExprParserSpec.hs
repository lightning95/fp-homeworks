 module AExprParserSpec (main, spec) where

import           AExpr                 (AExpr (..))
import           AExprParser           (aExprParser)
import           Test.Hspec
import           Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import           Text.Megaparsec       (parse)

spec :: Spec
spec =
  it "aExprParser" $ do
  parse aExprParser "" "x + 3 * (let x = 2 in x)" `shouldParse`
    (Var "x" `Sum` (Const 3 `Mul` ("x" `Loc` Const 2 $ Var "x")))
  parse aExprParser "" "y + 3 / (let ab = 12 * x in ab * 3)" `shouldParse`
    (Var "y" `Sum` (Const 3 `Div` ("ab" `Loc` (Const 12 `Mul` Var "x") $ Var "ab" `Mul` Const 3)))
  parse aExprParser "" `shouldFailOn` "- 2"
  parse aExprParser "" `shouldFailOn` ","
  parse aExprParser "" `shouldFailOn` "let * 2"
  parse aExprParser "" `shouldFailOn` "+ 2"
  parse aExprParser "" "let2 * in3 - 3" `shouldParse`
    ((Var "let2" `Mul` Var "in3") `Sub` Const 3)
  parse aExprParser "" "a ^ b ^ c ^ (a + b * c ^ d)" `shouldParse`
    (Var "a" `Pow` (Var "b" `Pow` (Var "c" `Pow` (Var "a" `Sum` (Var "b" `Mul` (Var "c" `Pow` Var "d"))))))

main :: IO ()
main = hspec spec
