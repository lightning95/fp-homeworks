module AParser
  ( Parser (..)
  , satisfy
  , char
  , posInt
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  , empty
  , (<|>)
  ) where

import           Control.Applicative (Alternative (..), empty)
import           Data.Char           (isDigit, isUpper)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p
  -- = Parser $ \s -> first f <$> p s

instance Applicative Parser where
  pure a                  = Parser $ \s -> Just (a, s)
  Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
      Just (res, rest) -> first res <$> p2 rest
      _                -> Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = abParser *> pure ()

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> posInt <* char ' ' <*> posInt

instance Alternative Parser where
  -- empty :: f a
  empty = Parser $ const Nothing
  -- (<|>) :: f a -> f a -> f a
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

intOrUppercase :: Parser ()
intOrUppercase = posInt *> pure () <|> satisfy isUpper *> pure ()

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Just (res, rest) -> runParser (f res) rest
    _                -> Nothing
