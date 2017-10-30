module SExpr
    ( zeroOrMore
    , oneOrMore
    , spaces
    , ident
    , parseSExpr
    ) where

import           AParser   (Parser, char, posInt, satisfy, (<|>))
import           Data.Char (isAlpha, isAlphaNum, isSpace)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer
          | I Ident
          deriving (Show)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces

parseAtom :: Parser SExpr
parseAtom = A <$> ((N <$> posInt) <|> (I <$> ident))

parseComb :: Parser SExpr
parseComb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')
