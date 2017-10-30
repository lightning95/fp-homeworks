module OptionalMonadsSpec (main, spec) where

import           Test.Hspec

import           OptionalMonads


spec :: Spec
spec =
  it "eval" $ do
  eval (Const 1) `shouldBe` Right 1
  eval (Const 1 `Sum` Const 2) `shouldBe` Right 3
  eval (Const 1 `Div` ((Const 1 `Pow` Const 0) `Sum` Const (-1))) `shouldBe` Left DivisionByZero
  eval (Const 1 `Pow` (Const 1 `Mul` Const (-10))) `shouldBe` Left PowerToNegative

main :: IO ()
main = hspec spec
