{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Expr
  ( Expr(..)
  , ExprEval(..)
  , Ident
  , Bindings
  , EvalError
  , eval
  ) where

import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Reader.Class (MonadReader (..), asks, local)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Either.Utils          (maybeToEither)
import qualified Data.Map.Strict            as Map (Map, insert, lookup)

type Ident = String

data Expr = Const Integer
          | Var Ident
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Loc Ident Expr Expr
          deriving (Show, Eq)

type Bindings = Map.Map String Integer

data EvalError = DivisionByZero
               | PowerToNegative Integer
               | UnknownIdentifier String
               deriving (Show, Eq)

newtype ExprEval a = ExprEval
  { runExprEval :: ReaderT Bindings (Either EvalError) a
  } deriving (Functor, Applicative, Monad, MonadReader Bindings, MonadError EvalError)

eval :: forall m .
        ( Monad m
        , MonadReader Bindings m
        , MonadError EvalError m
        )
     => Expr -> m Integer
eval (Const x) = pure x
eval (Sum x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = div <$> eval x <*> right
  where
    right :: m Integer
    right = do
      res <- eval y
      if res == 0 then
        throwError DivisionByZero
      else
        pure res
eval (Pow x y) = do
  left  <- eval x
  right <- eval y
  if right < 0 then
    throwError $ PowerToNegative right
  else
    pure $ left ^ right
eval (Var name) = do
  res <- asks (Map.lookup name)
  maybeToEither (UnknownIdentifier name) res
eval (Loc name val expr) = do
  res <- eval val
  local (Map.insert name res) (eval expr)
