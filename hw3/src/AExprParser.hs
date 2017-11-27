{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module AExprParser
  ( Parser
  , aExprParser
  , symbol
  , rword
  , identifier
  ) where

import           AExpr                      (AExpr (..), Ident)
import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, many,
                                             notFollowedBy, try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space,
                                                  symbol)
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

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
rws = ["let", "in", "mut"]

identifier :: Parser Ident
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

aExprParser :: Parser AExpr
aExprParser = makeExprParser tokenParser aOperators

tokenParser :: Parser AExpr
tokenParser = lexeme constParser
  <|> varParser
  <|> parens (try locParser <|> aExprParser)

locParser :: Parser AExpr
locParser = do
  rword "let"
  ident <- identifier
  void (symbol "=")
  val <- aExprParser
  rword "in"
  right <- aExprParser
  pure $ Loc ident val right

constParser :: Parser AExpr
constParser = Const <$> integer

varParser :: Parser AExpr
varParser = Var <$> try identifier

aOperators :: [[Operator Parser AExpr]]
aOperators = [ [ InfixR (Pow <$ symbol "^")]
             , [ InfixL (Mul <$ symbol "*")
               , InfixL (Div <$ symbol "/")]
             , [ InfixL (Sum <$ symbol "+")
               , InfixL (Sub <$ symbol "-")]
             ]
