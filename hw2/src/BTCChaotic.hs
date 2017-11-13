{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module BTCChaotic
  ( Identity (..)
  , Const (..)
  ) where

import           Data.Functor (Functor, fmap)
import           Prelude      hiding (Either (..))

newtype Identity a = Identity { runIdentity :: a }
                   deriving (Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure               = Identity
  (<*>) (Identity f) = fmap f

instance Foldable Identity where
-- foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f b (Identity x) = f x b

instance Traversable Identity where
-- Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity x) = Identity <$> f x
---------------------------------------------
data Either a b = Left a
                | Right b
                deriving (Show)

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right $ f y

instance Applicative (Either a) where
  pure          = Right
  Left x <*> _  = Left x
  Right f <*> y = fmap f y

instance Foldable (Either a) where
  foldr _ z (Left  _) = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x)  = pure $ Left x
  traverse f (Right y) = Right <$> f y
------------------------------------
data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving (Show)

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tree where
  pure x                      = Node x (pure x) (pure x)
  Leaf         <*> _          = Leaf
  _            <*> Leaf       = Leaf
  Node f fl fr <*> Node x l r = Node (f x) (fl <*> l) (fr <*> r)

instance Foldable Tree where
-- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf         = z
  foldr f z (Node c l r) = foldr f (c `f` foldr f z r) l

instance Traversable Tree where
-- traverse :: (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r
----------------------------------------------------
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
-- (b -> c) -> Const a b -> Const a c
  fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
-- pure :: b -> Const a b
  pure _              = Const mempty
-- (<*>) :: Const a (b -> c) -> Const a b -> Const a c
  Const f <*> Const a = Const $ f `mappend` a

instance Foldable (Const a) where
-- foldr :: (b -> c -> c) -> c -> Const a b -> c
  foldr _ z _  = z

instance Traversable (Const a) where
-- traverse :: (a -> f c) -> Const a b -> f (Const c a)
  traverse _ (Const a) = pure $ Const a
--------------------------------------------
data Tuple a b = Tuple a b
               deriving (Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a $ f b

instance Monoid a => Applicative (Tuple a) where
  pure                      = Tuple mempty
  Tuple a b <*> Tuple a2 b2 = Tuple (a `mappend` a2) (b b2)

instance Foldable (Tuple a) where
-- foldr :: (b -> c -> c) -> c -> Tuple a b -> c
  foldr f z (Tuple _ b) = f b z

instance Traversable (Tuple a) where
-- traverse :: (b -> f c) -> Tuple a b -> f (Tuple a c)
  traverse f (Tuple a b) = Tuple a <$> f b
