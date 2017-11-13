module ChaoticProofs where

-- newtype Identity a = Identity { runIdentity :: a }
--                    deriving (Show)

-- instance Functor Identity where
--   fmap f (Identity x) = Identity $ f x


-- fmap (f . g) (Identity x)      ==  (fmap f . fmap g) (Identity x)
--    = Identity $ (f . g) x
--    = Identity $ f (g x)
-- 
-- (fmap f . fmap g) (Identity x)
--    = fmap f (fmap g (Identity x))
--    = fmap f (Identity $ g x)
--    = Identity $ f (g x)
----------------------------------
-- data Either a b = Left a
--                 | Right b
--                 deriving (Show)

-- instance Functor (Either a) where
--   fmap _ (Left  x) = Left x
--   fmap f (Right y) = Right $ f y

-- functor
-- fmap id x                   ==  id x  == id $ Right y == Right y
--   = Right $ id y
--   = Right y
--    
-- fmap id Left x              ==  id x  == id $ Left x == Left x
--   = Left x  
-----------------------------
-- data Tree a = Leaf
--             | Node a (Tree a) (Tree a)
--             deriving (Show)

-- instance Applicative Tree where
--   pure x                      = Node x Leaf Leaf
--   Leaf         <*> x          = x
--   _            <*> Leaf       = Leaf
--   Node f fl fr <*> Node x l r = Node (f x) (fl <*> l) (fr <*> r)

-- Applicative::identity
-- pure id <*> v                       = v
--   = Node id Leaf Leaf <*> Leaf 
--   = Leaf
--
--   = Node id Leaf Leaf <*> Node x l r 
--   = Node (id x) (Leaf <*> l) (Leaf <*> r)
--   = Node x l r
------------------------------
