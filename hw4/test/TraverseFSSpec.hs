module TraverseFSSpec
  ( main
  , spec
  ) where

import           Control.Lens ((^..), (^?))
import           FSLenses     (FS (..))
import           Test.Hspec
import           TraverseFS   (cd, fileM, ls)

spec :: Spec
spec = do
  it "ls, cd" $ do
    (a ^.. cd "b" . ls) `shouldBe` ["b.f", "c"]
    (a ^.. cd "b" . cd "c" . ls) `shouldBe` ["c.f"]
    (a ^.. cd "b" . cd "c" . cd "d" . ls) `shouldBe` []
    (a ^.. ls) `shouldBe` ["a.f", "b"]

  it "fileM, cd" $ do
    (a ^? cd "b" . fileM "b.f") `shouldBe` Just "b.f"
    (a ^? cd "b" . cd "c" . fileM "c.f") `shouldBe` Just "c.f"
    (a ^? cd "b" . cd "c" . cd "d" . fileM "xyz") `shouldBe` Nothing
    (a ^? fileM "a.f") `shouldBe` Just "a.f"
    (a ^? fileM "b") `shouldBe` Nothing

main :: IO ()
main = hspec spec
----------------------------------------
a :: FS
a = Dir "a" [File "a.f", Dir "b" [File "b.f", Dir "c" [File "c.f"]]]
