module VarsParser
  ( assignParser
  , Assign(..)
  ) where

import           AExpr           (AExpr, Ident)
import           AExprParser     (Parser, aExprParser, identifier, rword,
                                  symbol)
import           Control.Monad   (void)
import           Text.Megaparsec (try, (<|>))

data Assign = Crt Ident AExpr
            | Rsgn Ident AExpr
            | Write AExpr
            | Read Ident
            deriving (Show, Eq)

assignParser :: Parser Assign
assignParser = try readParser
  <|> try writeParser
  <|> try crtParser
  <|> rsgnParser

readParser :: Parser Assign
readParser = do
  void (symbol ">")
  ident <- identifier
  pure $ Read ident

writeParser :: Parser Assign
writeParser = do
  void (symbol "<")
  expr <- aExprParser
  pure $ Write expr

rsgnParser :: Parser Assign
rsgnParser = do
  ident <- identifier
  void (symbol "=")
  expr  <- aExprParser
  pure $ Rsgn ident expr

crtParser :: Parser Assign
crtParser = do
  rword "mut"
  ident <- identifier
  void (symbol "=")
  expr  <- aExprParser
  pure $ Crt ident expr
