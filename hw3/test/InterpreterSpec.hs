module InterpreterSpec (main, spec) where

import           AExpr           (AExpr (..), Bindings, EvalError (..), Ident)
import qualified Data.Map.Strict as Map (empty, insert)
import           Interpreter     (interpret)
import           Test.Hspec
import qualified Vars            as V (LangError (..))
import           VarsParser      (Assign (..))

spec :: Spec
spec =
  it "interpret" $ do
    interpret [] `shouldBe`
      Right emp
    interpret [Crt "x" $ Const 2] `shouldBe`
      Right (ins "x" 2 emp)
    interpret [Crt "x" $ Const 2 `Sum` Const 3] `shouldBe`
      Right (ins "x" 5 emp)
    interpret [Crt "x" $ Const 2, Crt "y" $ Var "x"]
      `shouldBe` Right (ins "x" 2 $ ins "y" 2 emp)
    interpret [Crt "x" $ Const 2, Rsgn "x" $ Var "x" `Mul` Const 2]
      `shouldBe` Right (ins "x" 4 emp)
    interpret [Crt "x" $ "x" `Loc` Const 2 $ Var "x"]
      `shouldBe` Right (ins "x" 2 emp)
    interpret [Rsgn "x" $ Const 2] `shouldBe`
      Left (V.UnknownIdentifier "x")
    interpret [Crt "x" $ Const 2, Crt "x" $ Const 3] `shouldBe`
      Left (V.DuplicateIdentifier "x")
    interpret [Crt "x" $ Var "y"] `shouldBe`
      Left (V.EV $ UnknownIdentifier "y")

main :: IO ()
main = hspec spec
---------------
emp :: Bindings
emp = Map.empty

ins :: Ident -> Integer -> Bindings -> Bindings
ins = Map.insert
