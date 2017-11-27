{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Interpreter
  ( interpret
  ) where

import qualified AExpr                      as A (AExprEval (..), Bindings,
                                                  eval)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.State.Class  (MonadState (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State  (StateT (..))
import qualified Data.Map.Strict            as Map (empty)
import           Vars                       (LangError (..), Vars (..),
                                             createVar, reassignVar)
import           VarsParser                 (Assign (..))

interpret :: [Assign] -> Either LangError ((), A.Bindings)
interpret s = runStateT (runVars $ interpret' s) Map.empty

interpret' :: [Assign] -> Vars ()
interpret' []     = pure ()
interpret' (x:xs) = do
  s <- get
  let
    (e, f) = case x of
        (Crt ident expr)  -> (expr, createVar ident)
        (Rsgn ident expr) -> (expr, reassignVar ident)
  val <- case runReaderT (A.runAExprEval $ A.eval e) s of
      Right r -> pure r
      Left er -> throwError $ EV er
  _ <- f val
  interpret' xs