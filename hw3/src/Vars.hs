{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Vars
  ( createVar
  , reassignVar
  , Vars(..)
  , LangError(..)
  ) where

import           AExpr                     (Bindings, EvalError, Ident)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.State.Class (MonadState (..), modify)
import           Control.Monad.Trans.State (StateT (..))
import qualified Data.Map.Strict           as Map (insert, lookup)

data LangError = DuplicateIdentifier Ident
               | UnknownIdentifier Ident
               | EV EvalError
               deriving (Show, Eq)

newtype Vars a = Vars
  { runVars :: StateT Bindings (Either LangError) a
  } deriving (Functor, Applicative, Monad, MonadState Bindings,
              MonadError LangError)

createVar :: forall m .
           ( Monad m
           , MonadError LangError m
           , MonadState Bindings m
           )
          => Ident -> Integer -> m ()
createVar name val = do
  inner <- get
  case Map.lookup name inner of
    Just _ -> throwError $ DuplicateIdentifier name
    _      -> modify $ Map.insert name val


reassignVar :: forall m .
             ( Monad m
             , MonadError LangError m
             , MonadState Bindings m
             )
            => Ident -> Integer -> m ()
reassignVar name val = do
  inner <- get
  case Map.lookup name inner of
    Just _ -> modify $ Map.insert name val
    _      -> throwError $ UnknownIdentifier name
