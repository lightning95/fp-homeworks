{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BTCFishesMonad
  ( returnFish
  , returnJoin
  , (>=>)
  , join
  ) where

import           Data.Function (id)

class Monad m where
  return     :: a -> m a
  (>>=)      :: m a -> (a -> m b) -> m b

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)
               -- m b -> (b -> m c) -> m (m c)
               -- m (m a) -> m a

instance Monad m => MonadFish m where
  returnFish  = return
  (>=>) f t a = f a >>= t

-- LAWS MONAD
-- 1. m >>= return    ≡ m
-- 2. return a >>= f  ≡ f a
-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- LAWS
-- 1. (f >=> returnFish) a ≡ f a
-- 2. returnFish >=> f ≡ f
-- 3. (f >=> g) >=> h  ≡ f >=> (g >=> h)

-- 1) (f >=> returnFish) a   -> f a
--    = f a >>= returnFish           (returnFish ≡ return)
--    = f a >>= return               (Monad 2)
--    = f a 
---------------------------------
class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a
             -- (m a) -> (a -> m b) -> m b
             -- m a -> (a -> m b) -> m (m b)

instance Monad m => MonadJoin m where
  returnJoin = return
  join       = (>>= id)

-- LAWS MONAD
-- 1. m >>= return    ≡ m
-- 2. return a >>= f  ≡ f a
-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- LAWS MONADJOIN
-- 1. join . pure            ≡ id
-- 2. join . fmap returnJoin ≡ id
-- 3. join . fmap join       ≡ join . join

-- 1) (join . pure) a          -> id a
--    = ((>>= id) . return) a   (.)
--    = (>>= id) $ return a     ($)
--    = return a >>= id         (MONAD 2)
--    = id a
