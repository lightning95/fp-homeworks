{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module InterpreterIO
  ( interpretIO
  ) where

import qualified AExpr                      as A (AExprEval (..), Bindings,
                                                  Ident, eval)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Class  (MonadState (..), modify)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State  (StateT (..), execStateT)
import qualified Data.Map.Strict            as Map (empty, insert, lookup)
import           Vars                       (LangError (..))
import           VarsParser                 (Assign (..))

data InterpreterError = InterpreterError LangError Integer
  deriving (Show, Eq)

newtype Lang a = Lang
  { runLang :: StateT A.Bindings (ExceptT InterpreterError IO) a
  } deriving (Functor, Applicative, Monad, MonadState A.Bindings, MonadError InterpreterError, MonadIO)

interpretIO :: [Assign] -> IO (Either InterpreterError A.Bindings)
interpretIO s = runExceptT $ execStateT (runLang $ interpretIO' s 0) Map.empty

interpretIO' :: forall m .
             ( Monad m
             , MonadError InterpreterError m
             , MonadState A.Bindings m
             , MonadIO m
             )
            => [Assign] -> Integer -> m ()
interpretIO' []     _  = pure ()
interpretIO' (x:xs) i  = do
  s <- get
  let
    (e, f) = case x of
        (Crt ident expr)  -> (expr, createVar ident)
        (Rsgn ident expr) -> (expr, reassignVar ident)
        (Write expr)      -> (expr, writeVar)
        _                 -> undefined
  case x of
    (Read ident) -> readVar ident i
    _            -> do
      val <- case runReaderT (A.runAExprEval $ A.eval e) s of
        Right r -> pure r
        Left er -> throwError $ InterpreterError (EV er) i
      _ <- f val i
      pure ()
  interpretIO' xs (i + 1)

writeVar :: forall m .
          ( MonadIO m
          )
         => Integer -> Integer -> m ()
writeVar x _  = liftIO $ print x

readVar :: forall m .
         ( Monad m
         , MonadIO m
         , MonadError InterpreterError m
         , MonadState A.Bindings m
         )
        => A.Ident -> Integer -> m ()
readVar name line = do
  s <- liftIO getLine
  inner <- get
  let val = read s
  case Map.lookup name inner of
    Just _ -> modify $ Map.insert name val
    _      -> throwError $ InterpreterError (UnknownIdentifier name) line

createVar :: forall m .
           ( Monad m
           , MonadError InterpreterError m
           , MonadState A.Bindings m
           )
          => A.Ident -> Integer -> Integer -> m ()
createVar name val line = do
  inner <- get
  case Map.lookup name inner of
    Just _ -> throwError $ InterpreterError (DuplicateIdentifier name) line
    _      -> pure ()
  modify $ Map.insert name val

reassignVar :: forall m .
             ( Monad m
             , MonadError InterpreterError m
             , MonadState A.Bindings m
             )
            => A.Ident -> Integer -> Integer -> m ()
reassignVar name val line = do
  inner <- get
  case Map.lookup name inner of
    Just _ -> modify $ Map.insert name val
    _      -> throwError $ InterpreterError (UnknownIdentifier name) line
