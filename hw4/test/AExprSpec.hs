module AExprSpec (main, spec) where

import           AExpr                      (AExpr (..), Bindings,
                                             EvalError (..), eval)
import           Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Map.Strict            as Map (empty, insert)
import           Test.Hspec

spec :: Spec
spec =
  it "eval" $ do
    helper (Const 1) Map.empty
      `shouldBe` Right 1
    helper (Var "x") mapWithX1
      `shouldBe` Right 1
    helper (Var "x") Map.empty
      `shouldBe` Left (UnknownIdentifier "x")
    helper (Var "x" `Sum` Var "x") mapWithX1
      `shouldBe` Right 2
    helper ("x" `Loc` Const 2 $ Var "x") Map.empty
      `shouldBe` Right 2
    helper (Var "x" `Sum` (Const 3 `Mul` ("x" `Loc` Const 2 $ Var "x"))) mapWithX1
      `shouldBe` Right 7
    helper (Var "x" `Pow` Const (-2)) mapWithX1
      `shouldBe` Left (PowerToNegative $ -2)
    helper (Var "x" `Div` Const 0) mapWithX1
      `shouldBe` Left DivisionByZero

main :: IO ()
main = hspec spec
----------------------------------------
mapWithX1 :: Bindings
mapWithX1 = Map.insert "x" 1 Map.empty

helper :: AExpr -> Bindings -> Either EvalError Integer
helper = runReaderT . eval
