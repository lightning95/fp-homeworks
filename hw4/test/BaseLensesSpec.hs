module BaseLensesSpec
  ( main
  , spec
  ) where

import           BaseLenses (over, set, view, _1, _2)
import           Test.Hspec

spec :: Spec
spec = do
  it "set, _1, _2" $ do
    set _1 (1::Int) (2, 3::Int) `shouldBe` (1, 3)
    set _2 (1::Int) (2, 3::Int) `shouldBe` (2, 1)
    set _1 3 (set _1 (1::Int) (2, 3::Int)) `shouldBe` (3, 3)
    set _2 3 (set _2 (1::Int) (2, 0::Int)) `shouldBe` (2, 3)

  it "view, _1, _2" $ do
    view _1 (2, 3::Int) `shouldBe` 2
    view _2 (2, 3::Int) `shouldBe` 3
    view _1 (view _1 ((1, 2), 3::Int)) `shouldBe` 1
    view _2 (view _2 (1, (2, 3))) `shouldBe` 3

  it "over, _1, _2" $ do
    over _1 (+1) (2, 3) `shouldBe` (3, 3)
    over _2 (+1) (2, 3) `shouldBe` (2, 4)
    over (_1._2) (+1) ((1, 2), 3) `shouldBe` ((1, 3), 3)
    over (_2._1) (+1) (1, (2, 3)) `shouldBe` (1, (3, 3))


main :: IO ()
main = hspec spec
----------------------------------------

