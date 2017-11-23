{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module ExprParser where

import           Control.Applicative      (empty)
import           Control.Monad            (void)
import           Data.Void                (Void)
import           Expr                     (Expr (..), Ident)
import           Text.Megaparsec
import           Text.Megaparsec.Char     
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer    as L

import           Control.Monad.State.Lazy

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
 
integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["let", "in"]

identifier :: Parser Ident
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

exprParser :: Parser Expr
exprParser = between sc eof expr

expr :: Parser Expr
expr = parens exprParser
  <|> parens locParser
  <|> binOpParser
  <|> constParser
  <|> varParser

locParser :: Parser Expr
locParser = do
  rword "let"
  ident <- identifier
  void (symbol "=")
  val <- exprParser
  rword "in"
  right <- exprParser
  pure $ Loc ident val right

constParser :: Parser Expr
constParser = Const <$> integer

varParser :: Parser Expr
varParser = Var <$> identifier

binOpParser :: Parser Expr
binOpParser = makeExprParser exprParser aOperators

aOperators :: [[Operator Parser Expr]]
aOperators = [ [ InfixR (Pow <$ symbol "^")]
             , [ InfixL (Mul <$ symbol "*")
               , InfixL (Div <$ symbol "/")]
             , [ InfixL (Sum <$ symbol "+")
               , InfixL (Sub <$ symbol "-")]
             ]
