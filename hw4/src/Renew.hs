{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Renew
  (
  ) where

import Control.Comonad (Comonad(..))

data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance MonoidAction Int [a] where
  act s e = length e + s

instance Functor (Renew s e) where
--  fmap :: a -> b -> Renew s e a -> Renew s e b
  fmap f (Renew t s) = Renew (f . t) s

instance MonoidAction s e => Comonad (Renew s e) where
  -- extract   :: Renew s e a -> a
  extract   (Renew t s)   = t mempty
  
  -- extend    :: (Renew s e a -> b) -> Renew s e a -> Renew s e b
  extend f  (Renew t s) = Renew (\e -> f (Renew t s)) s
  
  -- duplicate :: Renew s e a -> Renew s e (Renew s e a)
  duplicate r@(Renew t s) = undefined



-- data Store s a = Store (s -> a) s

-- instance Comonad (Store s) where
--     extract :: Store s a -> a
--     extract  (Store f s) = f s

--     extend :: (Store s a -> b) -> Store s a -> Store s b
--     extend f (Store g s) = Store (f . Store g) s 

-- extend extract (Renew t s) = id
--  = Renew (\e -> extract (Renew t $ act s e)) s
--  = Renew (\e -> t mempty) s
--  = 
