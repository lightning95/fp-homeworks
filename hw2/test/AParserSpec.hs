module AParserSpec (main, spec) where

import           AParser    (Parser (..), abParser, abParser_, char,
                             intOrUppercase, intPair, posInt, satisfy)
import           Data.Char  (isDigit, isUpper)

import           Test.Hspec


spec :: Spec
spec = do
  it "satisfy" $ do
    runParser (satisfy isDigit) "1a" `shouldBe` Just('1', "a")
    runParser (satisfy (== '2')) "123" `shouldBe` Nothing
    runParser (satisfy (/= '1')) "23" `shouldBe` Just('2', "3")
    runParser (satisfy isUpper) "ABC" `shouldBe` Just ('A',"BC")
    runParser (satisfy isUpper) "abc" `shouldBe` Nothing
---------------------------------
  it "char " $ do
    runParser (char 'a') "aaaa" `shouldBe` Just ('a', "aaa")
    runParser (char 'b') "aaaa" `shouldBe` Nothing
    runParser (char 'x') "xyz" `shouldBe` Just ('x',"yz")
---------------------------------
  it "posInt" $ do
    runParser posInt "1111 " `shouldBe` Just (1111, " ")
    runParser posInt "abc" `shouldBe` Nothing
---------------------------------
  it "abParser" $ do
    runParser abParser "ab123" `shouldBe` Just (('a', 'b'), "123")
    runParser abParser "123" `shouldBe` Nothing
    runParser abParser "aa123" `shouldBe` Nothing
    runParser abParser "ba123" `shouldBe` Nothing
    runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")
    runParser abParser "aebcdf" `shouldBe` Nothing
--------------------------------
  it "abParser_" $ do
    runParser abParser_ "ab123" `shouldBe` Just ((), "123")
    runParser abParser_ "123" `shouldBe` Nothing
    runParser abParser_ "aa123" `shouldBe` Nothing
    runParser abParser_ "ba123" `shouldBe` Nothing
    runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")
    runParser abParser_ "aebcdf" `shouldBe` Nothing
--------------------------------
  it "intPair" $ do
    runParser intPair "123 12abc" `shouldBe` Just ([123, 12], "abc")
    runParser intPair "123 abc" `shouldBe` Nothing
    runParser intPair "00abc" `shouldBe` Nothing
    runParser intPair "0 0" `shouldBe` Just ([0,0], "")
    runParser intPair "12 34" `shouldBe` Just ([12,34],"")
--------------------------------
  it "intOrUppercase" $ do
    runParser intOrUppercase "123Abc" `shouldBe` Just ((), "Abc")
    runParser intOrUppercase "Abc" `shouldBe` Just ((), "bc")
    runParser intOrUppercase "abc" `shouldBe` Nothing
    runParser intOrUppercase "+-" `shouldBe` Nothing
    runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
    runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
    runParser intOrUppercase "foo" `shouldBe` Nothing

main :: IO ()
main = hspec spec
