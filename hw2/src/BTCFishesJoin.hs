
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BTCFishesJoin
  ( (>=>)
  , return
  , (>>=)
  , returnFish
  ) where

import           Data.Function (flip, (.))
import           Data.Functor  (Functor(..))

-- instance Monad m => Functor m where
--   fmap f m = m >>= (return . f)

-- LAWS MONAD
-- 1. m >>= return    ≡ m
-- 2. return a >>= f  ≡ f a
-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- LAWS FUNCTOR
-- 1) fmap id a       ==  id a
-- 2) fmap (f . g) a  ==  (fmap f . fmap g) a

-- 1) fmap id a                 -> id a
--    = a >>= (return . id)     (f . id)
--    = a >>= return            (MONAD 1)
--    = a
-- 2) fmap (f . g) a            <-> (fmap f . fmap g) a
--    = a >>= (return . (f . g))
--
--    (fmap f . fmap g) a
--    = fmap f (fmap g a)
--    = fmap f (a >>= (return . g))
--    = (a >>= (return . g)) >>= (return . f)
--    =  a >>= (\x -> (return . g) x >>= (return . f))
--    =  a >>= (\x -> return (g x) >>= (return . f))
--    =  a >>= (\x -> (return . f) (g x))
--    =  a >>= (\x -> (return . f . g) x)
--    =  a >>= (return . (f . g))

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

class Monad m where
  return     :: a -> m a
  (>>=)      :: m a -> (a -> m b) -> m b

--`blackbird` by Amar Shah
(...) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(...) = (.) . (.)

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin
  (>>=)  = join ... flip fmap

-- LAWS MONADJOIN
-- 1. join . returnJoin      ≡ id
-- 2. join . fmap returnJoin ≡ id
-- 3. join . fmap join       ≡ join . join

-- LAWS MONAD
-- 1. m >>= return    ≡ m
-- 2. return a >>= f  ≡ f a
-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- 1) m >>= return         -> m
--    = (join ... flip fmap) m returnJoin    (...)
--    = join $ fmap returnJoin m             ($)
--    = (join . fmap returnJoin) m           (MONADJOIN 2)
--    = id m
--    = m
class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  f >=> t    = join . fmap t . f

-- LAWS MONADJOIN
-- 1. join . returnJoin      ≡ id
-- 2. join . fmap returnJoin ≡ id
-- 3. join . fmap join       ≡ join . join

-- LAWS MONADFISH
-- 1. (f >=> returnFish) a ≡ f a
-- 2. returnFish >=> f     ≡ f
-- 3. (f >=> g) >=> h      ≡ f >=> (g >=> h)

-- 1) (f >=> returnFish) a                 -> f a
--    = (join . fmap returnJoin . f) a
--    = ((join . fmap returnJoin) . f) a
--    = (id . f) a
--    = f a
