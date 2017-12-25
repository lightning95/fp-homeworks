{-# LANGUAGE TemplateHaskell #-}

module TmpHsSpec
  ( main
  , spec
  ) where

import           Data.Text (pack)
import           TmpHs (chooseByIndices)
import           Test.Hspec

import ShowT(mkShowT, showT)

----------------------------------------
data MyData = MyData
     { foo :: String
     , bar :: Int
     } deriving (Show)

mkShowT ''MyData

data MD = MD
     { lol      :: [String]
     , kek      :: Integer
     , cheburek :: Float
     } deriving (Show)

mkShowT ''MD
-------------------------------------------

spec :: Spec
spec = do
  it "chooseByIndices" $ do
    ($(chooseByIndices 4 [2, 0]) ("hello", 10, [4,3], 2)) `shouldBe`
      ([4, 3], "hello")
    $(chooseByIndices 3 [1, 1, 2, 1, 1]) ("hello", 10, [4,3]) `shouldBe`
      (10, 10, [4, 3], 10, 10)
    $(chooseByIndices 4 [1, 1, 3, 1, 1]) ("hello", 10, [4,3], 2) `shouldBe`
      (10, 10, 2, 10, 10)
    $(chooseByIndices 6 [5, 4, 3, 2, 1, 0]) ("0", 1, "2", 3, "4", 5) `shouldBe`
      (5, "4", 3, "2", 1, "0")

  it "should show datas as specials" $ do
    showT (MyData "" 1) `shouldBe`
      pack "ShowT: foo = \"\", bar = 1"
    showT (MD ["lel", "mel"] 1 0.1) `shouldBe`
      pack "ShowT: lol = [\"lel\",\"mel\"], kek = 1, cheburek = 0.1"
    showT (MyData "trolololo x" 3) `shouldBe`
      pack "ShowT: foo = \"trolololo x\", bar = 3"
    showT (MD ["everything", "is" , "connected"] 23 3.14) `shouldBe`
      pack "ShowT: lol = [\"everything\",\"is\",\"connected\"], kek = 23, cheburek = 3.14"

main :: IO ()
main = hspec spec
