{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module AExpr
  ( AExpr (..)
  , AExprEval (..)
  , Ident
  , Bindings
  , EvalError(..)
  , eval
  ) where

import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Reader.Class (MonadReader (..), asks, local)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Either.Utils          (maybeToEither)
import qualified Data.Map.Strict            as Map (Map, insert, lookup)

type Ident = String

type Bindings = Map.Map Ident Integer

data AExpr = Const Integer
          | Var Ident
          | Sum AExpr AExpr
          | Sub AExpr AExpr
          | Mul AExpr AExpr
          | Div AExpr AExpr
          | Pow AExpr AExpr
          | Loc Ident AExpr AExpr
          deriving (Show, Eq)

data EvalError = DivisionByZero
               | PowerToNegative Integer
               | UnknownIdentifier Ident
               deriving (Show, Eq)

newtype AExprEval a = AExprEval
  { runAExprEval :: ReaderT Bindings (Either EvalError) a
  } deriving (Functor, Applicative, Monad, MonadReader Bindings,
    MonadError EvalError)

eval :: forall m .
        ( Monad m
        , MonadReader Bindings m
        , MonadError EvalError m
        )
     => AExpr -> m Integer
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
