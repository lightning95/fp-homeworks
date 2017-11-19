module MonadParserSpec (main, spec) where

import           AParser     (Parser (..))
import           MonadParser (Input, LetExpr (..), Sum (..), doAll,
                              parseLetExpr)
import           Test.Hspec

spec :: Spec
spec = do
  it "parseLetExpr" $ do
   runParser parseLetExpr (head input) `shouldBe` Just (LE "x" [C 1, C 2, C 5], "")
   runParser parseLetExpr (input !! 1) `shouldBe` Just (LE "y" [I "x", I "x"], "")
   runParser parseLetExpr (last input) `shouldBe` Just (LE "z" [C 0, I "x", I "y", C 8], "")
   runParser parseLetExpr "let x = " `shouldBe` Nothing
   runParser parseLetExpr "let    = 1" `shouldBe` Nothing
   runParser parseLetExpr "  let  = x  " `shouldBe` Nothing
   runParser parseLetExpr "let x = 5 +" `shouldBe` Just (LE "x" [C 5], "+")
-------------------------------------
  it "doAll" $ do
    doAll input `shouldBe` [LE "x" [C 8], LE "y" [C 16], LE "z" [C 32]]
    doAll input2 `shouldBe` [LE "kek" [C 6], LE "lol" [C 1012], LE "lol2" [C 11019]]
  it "show" $ do
    map show (doAll input) `shouldBe` ["let x = 8", "let y = 16", "let z = 32"]
    map show (doAll input2) `shouldBe` ["let kek = 6", "let lol = 1012", "let lol2 = 11019"]

main :: IO ()
main = hspec spec
-----------------------------------------
input :: Input
input =  [ "    let x = 1 + 2 + 5      "
         , "    let   y = x + x   "
         , "let z=0+    x   + y +   8   "
         ]

input2 :: Input
input2 = [ " let kek=1   + 2 + 3   "
         , "let lol   = kek + kek + 1000"
         , "let lol2 = kek + lol + 10001"
         ]

-- inputWithNoValue :: Input
-- inputWithNoValue = [ "let x = y" ]
