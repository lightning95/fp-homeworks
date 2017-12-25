module ChangeFSSpec
  ( main
  , spec
  ) where

import           ChangeFS     (changeExt, rmDir, subDirs)
import           Control.Lens ((^.), (^..), (^?))
import           Data.Maybe   (fromJust)
import           FSLenses     (FS (..))
import           Test.Hspec
import           TraverseFS   (cd)

spec :: Spec
spec = do
  it "changeExt" $ do
    changeExt "f" "lel" (fromJust (a ^? cd "b")) `shouldBe`
      Dir "b" [ File "b.lel"
              , Dir "c.ru" [ File "c.f"]
              , File "kek.file"
              , File "lol.lel"
              ]
    changeExt "ru" "en" (fromJust $ a ^? cd "b") `shouldBe`
      Dir "b" [ File "b.f"
              , Dir "c.ru" [ File "c.f"]
              , File "kek.file"
              , File "lol.f"
              ]
  it "subDir" $ do
    subDirs a `shouldMatchList`
      [ "a.f"
      , "b" , "b.f"
            , "c.ru"
                     , "c.f"
            , "kek.file"
            , "lol.f"
      ]
    subDirs (fromJust (a ^? cd "b")) `shouldMatchList`
      [ "b.f"
      , "c.ru"
                , "c.f"
      , "kek.file"
      , "lol.f"
      ]
  it "rmDir" $ do
    rmDir "c" t `shouldBe`
      Dir "t" [File "a"]


main :: IO ()
main = hspec spec
----------------------------------------
a :: FS
a = Dir "a" [ File "a.f"
            , Dir "b" [ File "b.f"
                      , Dir "c.ru" [ File "c.f"]
                      , File "kek.file"
                      , File "lol.f"
                      ]
            ]

t :: FS
t = Dir "t" [Dir "c" [], File "a"]
