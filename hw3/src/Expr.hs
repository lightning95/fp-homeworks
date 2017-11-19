{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Expr where

import           Control.Applicative  (liftA2)
import           Control.Monad.Reader (Reader, asks, local)
import           Data.Either.Utils    (maybeToEither)
import qualified Data.Map.Strict      as Map (Map, lookup, insert)

type Ident = String

data Expr = Const Int
          | Var Ident
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Loc Ident Expr Expr
          deriving (Show, Eq)

type Bindings = Map.Map String Int

data EvalError = DivisionByZero
               | PowerToNegative
               | UnknownIdentifier String
               deriving (Show, Eq)

type Result = Either EvalError Int

evalR :: Expr -> Reader Bindings Result
evalR (Const x) = pure $ pure x
evalR (Sum x y) = liftA2 (+) <$> evalR x <*> evalR y
evalR (Sub x y) = liftA2 (-) <$> evalR x <*> evalR y
evalR (Mul x y) = liftA2 (*) <$> evalR x <*> evalR y
evalR (Div x y) = liftA2 div <$> evalR x <*> right
  where
    right :: Reader Bindings Result
    right = do
      res <- evalR y
      pure $ case res of
        Right 0 -> Left DivisionByZero
        r       -> r
evalR (Pow x y) = do
  left  <- evalR x
  right <- evalR y
  let right2 = case right of
        Right r | r < 0 -> Left PowerToNegative
        e               -> e
  pure ((^) <$> left <*> right2)
evalR (Var name) = asks $ \b ->
  maybeToEither (UnknownIdentifier name) (Map.lookup name b)
evalR (Loc name val expr) = do
  res <- evalR val
  case res of
    l@(Left _) -> pure l
    Right r    -> local (Map.insert name r) (evalR expr)
