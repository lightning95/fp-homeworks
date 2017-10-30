{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BaseTypeClassesFishes
  (
  ) where

import           Data.Function (flip, const, id, ($), (.))
import           Data.Functor  (Functor(..), (<$>))

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-# LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    #-}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)
                 -- m b -> (b -> m c) -> m (m c)
                 -- m (m a) -> m a
    {-# LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    #-}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

               -- (m a) -> (a -> m b) -> m b

               -- m a -> (a -> m b) -> m (m b)

    {-# LAWS
        1. join . pure            ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
    #-}


-- instance Monad     m => MonadFish m where
--   returnFish  = return
--   (>=>) f t a = f a >>= t

-- instance Monad     m => MonadJoin m where
--   returnJoin = return
--   join       = (>>= id)

-- instance MonadFish m => Monad     m where
--   return  = returnFish
--   (>>=) a f =  (const a >=> f) ()

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join    a  = (const a >=> id) ()


--`blackbird` by Amar Shah
(...) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(...) = (.) . (.)

instance (Functor m, MonadJoin m) => Monad     m where
  return  = returnJoin
  (>>=) = join ... flip fmap

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  f >=> t   = join . fmap t . f

