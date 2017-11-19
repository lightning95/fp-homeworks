{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module MonadParser
  ( parseLetExpr
  , parseInput
  , optimize
  , doAll
  , getExprs
  , LetExpr(..)
  , Sum(..)
  , Ident
  , Input
  ) where

import           AParser       (Parser (..), char, posInt, (<|>))
import qualified Data.Map.Lazy as Map
import           SExpr         (ident, spaces, zeroOrMore)

type Ident = String

data Sum = C Integer
         | I Ident
         deriving (Eq)

data LetExpr = LE Ident [Sum]
             deriving (Eq)

parseLetExpr :: Parser LetExpr
parseLetExpr = spaces *> parseLet *> (LE <$> ident <* spaces <* char '=' <* spaces
                                      <*> parseRight) <* spaces

parseLet :: Parser ()
parseLet = char 'l' *> char 'e' *> char 't' *> spaces *> pure ()

parseP :: Parser Sum
parseP = spaces *> ((C <$> posInt) <|> (I <$> ident)) <* spaces

parseRight :: Parser [Sum]
parseRight = (:) <$> parseP <*> zeroOrMore (spaces *> char '+' *> parseP)
-------------------------------------------------------
type Input = [String]

parseInput :: Input -> [Maybe (LetExpr, String)]
parseInput = map $ runParser parseLetExpr

getExprs :: [Maybe(LetExpr, String)] -> [LetExpr]
getExprs = let f :: Maybe (LetExpr, String) -> LetExpr
               f (Just (le, "")) = le
               f Nothing         = error "parsing error"
               f (Just (_, ret)) = error ret
           in map f

optimize :: [LetExpr] -> [LetExpr]
optimize = go Map.empty
  where
    go :: Map.Map Ident Integer -> [LetExpr] -> [LetExpr]
    go _ []            = []
    go m (LE name v:xs) = LE name [C res] : go (Map.insert name res m) xs
      where
        parse :: Sum -> Integer
        parse (C x) = x
        parse (I s) = case Map.lookup s m of
                        Just x -> x
                        _      -> error "No value in a map"
        res :: Integer
        res = sum $ map parse v

doAll :: Input -> [LetExpr]
doAll = optimize . getExprs . parseInput

instance Show LetExpr where
  show (LE s p) = "let " ++ s ++ " = " ++ sh p
    where
      sh :: [Sum] -> String
      sh []     = ""
      sh [x]    = show x
      sh (x:xs) = show x ++ " + " ++ sh xs

instance Show Sum where
  show (C x) = show x
  show (I x) = x
