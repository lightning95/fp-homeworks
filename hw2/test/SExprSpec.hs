module SExprSpec (main, spec) where

import           AParser    (Parser (..), posInt, satisfy)
import           Data.Char  (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import           SExpr      (Atom (..), SExpr (..), ident, oneOrMore,
                             parseSExpr, spaces, zeroOrMore)
import           Test.Hspec

spec :: Spec
spec = do
  it "zeroOrMore" $ do
    runParser (zeroOrMore (satisfy isDigit)) "123a" `shouldBe` Just("123", "a")
    runParser (zeroOrMore (satisfy isSpace)) "1a" `shouldBe` Just("", "1a")
    runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")
------------------------
  it "oneOrMore" $ do
    runParser (oneOrMore (satisfy isDigit)) "123a" `shouldBe` Just("123", "a")
    runParser (oneOrMore (satisfy isSpace)) "1a" `shouldBe` Nothing
    runParser (oneOrMore (satisfy isAlphaNum)) "123a " `shouldBe` Just("123a", " ")
    runParser (oneOrMore (satisfy isAlpha)) "abcA1" `shouldBe` Just("abcA", "1")
    runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
------------------------
  it "zeroOrMore" $ do
    runParser spaces "       1" `shouldBe` Just("       ", "1")
    runParser spaces "1a" `shouldBe` Just("", "1a")
    runParser (spaces *> posInt) " 345" `shouldBe` Just (345, "")
-----------------------
  it "ident" $ do
    runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
    runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
    runParser ident "2bad" `shouldBe` Nothing
    runParser ident "" `shouldBe` Nothing
-----------------------
  it "parseSExpr" $ do
    runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
    runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
    runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
      Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")
    runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe`
      Just (Comb [Comb [Comb [A (I "lambda"), A (I "x"), Comb [A (I "lambda"), A (I "y"),
          Comb [A (I "plus"), A (I "x"), A (I "y")]]], A (N 3)], A (N 5)], "")
    runParser parseSExpr
      "     (       lots    of   (   spaces   in   )   this  (   one   )   )   "
      `shouldBe`
      Just (Comb [A (I "lots"), A (I "of"), Comb [A (I "spaces"), A (I "in")], A (I "this"),
                  Comb [A (I "one")]], "")

main :: IO ()
main = hspec spec
