{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module BaseLenses
  ( set
  , view
  , over
  , lens
  -- , lens'
  , choosing
  , (<%~)
  , (<<%~)
  ) where

import           Control.Applicative   (Const (..))
import           Data.Functor.Identity (Identity (..))

type Lens  s t a b = forall f . Functor f => (a -> f b) -> s -> f t
-- Lens s s a a = forall f . Functor f => (a -> f a) -> s -> f s
type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set lenz f o = runIdentity $ lenz (Identity . const f) o

view :: Lens' s a -> s -> a              -- lookup value (getter)
view lenz o = getConst $ lenz Const o

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over lenz f o = runIdentity $ lenz (Identity . f) o


-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a -- = flip (,) x <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x, ) <$> f a -- (,) x <$> f a

-- TODO UNIT TESTS FOR _1, _2

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

-- -- Lens s s a a = forall f . Functor f => (a -> f a) -> s -> f s
-- lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
-- lens' getter setter = \f s -> setter s <$> f (getter s)

-- Объединить две линзы в одну, которая работает с Either.
-- (a -> f b) -> s1 -> f t1
-- (a -> f b) -> s2 -> f t2
-- (a -> f b) -> Either s1 s2 -> f (Either t1 t2)
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _  f (Left  s1) = Left  <$> l1 f s1
choosing _  l2 f (Right s2) = Right <$> l2 f s2

-- Изменить цель линзы и вернуть новый результат.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = ( getConst $ l (Const . f) s
              , runIdentity $ l (Identity . f) s
              )

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = ( getConst $ l Const s
               , runIdentity $ l (Identity . f) s
               )
