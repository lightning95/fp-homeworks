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

import           Control.Monad.Trans.State  (StateT (..))
import qualified Data.Map.Strict            as Map (empty, insert)
import           Vars                       (LangError (..), createVar,
                                             reassignVar)
import           VarsParser                 (Assign (..))

newtype Lang a = Lang
  { runLang :: StateT A.Bindings (ExceptT LangError IO) a
  } deriving (Functor, Applicative, Monad, MonadState A.Bindings, MonadError LangError, MonadIO)

interpretIO :: [Assign] -> IO (Either LangError ((), A.Bindings))
interpretIO s = runExceptT $ runStateT (runLang $ interpretIO' s) Map.empty

interpretIO' :: forall m .
             ( Monad m
             , MonadError LangError m
             , MonadState A.Bindings m
             , MonadIO m
             )
            => [Assign] -> m ()
interpretIO' []     = pure ()
interpretIO' (x:xs) = do
  s <- get
  let
    (e, f) = case x of
        (Crt ident expr)  -> (expr, createVar ident)
        (Rsgn ident expr) -> (expr, reassignVar ident)
        (Write expr)      -> (expr, writeVar)
        _                 -> undefined
  case x of
    (Read ident) -> readVar ident
    _            -> do
      val <- case runReaderT (A.runAExprEval $ A.eval e) s of
        Right r -> pure r
        Left er -> throwError $ EV er
      _ <- f val
      pure ()
  interpretIO' xs

writeVar :: forall m .
          ( MonadIO m
          )
         => Integer -> m ()
writeVar = liftIO . print

readVar :: forall m .
         ( MonadIO m
         , MonadState A.Bindings m
         )
        => A.Ident -> m ()
readVar name = do
  val <- liftIO getLine
  modify $ Map.insert name (read val)
