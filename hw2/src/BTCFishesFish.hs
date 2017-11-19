{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BTCFishesFish
  ( return
  , (>>=)
  , returnJoin
  , join
  ) where

import           Data.Function (const, id)

class Monad m where
  return     :: a -> m a
  (>>=)      :: m a -> (a -> m b) -> m b

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)
               -- m b -> (b -> m c) -> m (m c)
               -- m (m a) -> m a

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a
             -- (m a) -> (a -> m b) -> m b
             -- m a -> (a -> m b) -> m (m b)

instance MonadFish m => Monad     m where
  return  = returnFish
  a >>= f = (const a >=> f) ()

-- LAWS
-- 1. (f >=> returnFish) a ≡ f a
-- 2. (returnFish >=> f) a ≡ f a
-- 3. ((f >=> g) >=> h) a  ≡ (f >=> (g >=> h)) a

-- LAWS MONAD
-- 1. m >>= return    ≡ m
-- 2. return a >>= f  ≡ f a
-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- 1) m >>= return                     -> m
--    = (const m >=> returnFish) ()    (LAWS 1)
--    = const m ()                     (const)
--    = m

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join a     = (const a >=> id) ()
-- \x -> const a >>= return id
-- LAWS
-- 1. (f >=> returnFish) a ≡ f a
-- 2. (returnFish >=> f) a ≡ f a
-- 3. ((f >=> g) >=> h) a  ≡ (f >=> (g >=> h)) a

-- LAWS MONADJOIN
-- 1. (join . pure) a        ≡ id a
-- 2. join . fmap returnJoin ≡ id
-- 3. join . fmap join       ≡ join . join

-- 1) (join . pure) a                                  -> id a
--    = ((\b -> (const b >=> id) ()) . returnFish) a    (.)
--    = \b -> (const b >=> id) () $ returnFish a        ($)
--    = (const (returnFish a) >=> id) ()                
--
--  id a
--    = (returnFish >=> id) a
--    =  
