{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module MonadParser
  ( parseLetExpr
  , parseInput
  , optimize
  , getExprs
  , doAll
  ) where

import           AParser       (Parser (..), char, posInt, (<|>))
import qualified Data.Map.Lazy as Map
import           SExpr         (ident, spaces, zeroOrMore)

type Ident = String

data P = N Integer
       | R Ident
       deriving (Show)

data LetExpr = I String [P]
             deriving (Show)

parseLetExpr :: Parser LetExpr
parseLetExpr = spaces *> parseLet *> (I <$> ident <* spaces <* char '=' <* spaces <*> parseRight) <* spaces

parseLet :: Parser ()
parseLet = char 'l' *> char 'e' *> char 't' *> spaces *> pure ()

parseP :: Parser P
parseP = spaces *> ((N <$> posInt) <|> (R <$> ident)) <* spaces

parseRight :: Parser [P]
parseRight = (:) <$> parseP <*> zeroOrMore (spaces *> char '+' *> parseP)
-------------------------------------------------------
type Input = [String]

parseInput :: Input -> [Maybe (LetExpr, String)]
parseInput = map $ runParser parseLetExpr

getExprs :: [Maybe(LetExpr, String)] -> [LetExpr]
getExprs = let f (Just (le, "")) = le
               f Nothing         = error "parsing error"
               f (Just (_, ret)) = error ret
           in map f

optimize :: [LetExpr] -> [LetExpr]
optimize = go Map.empty
  where
    go :: Map.Map Ident Integer -> [LetExpr] -> [LetExpr]
    go _ []            = []
    go m (I name v:xs) = I name [N res] : go (Map.insert name res m) xs
      where
        parse (R s) = case Map.lookup s m of
                        Just x -> x
                        _      -> error "No value in a map"
        parse (N x) = x
        res         = sum $ map parse v

doAll :: Input -> [LetExpr]
doAll = optimize . getExprs . parseInput
