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
--   pure x                      = Node x (pure x) (pure x)
--   Leaf         <*> _          = Leaf
--   _            <*> Leaf       = Leaf
--   Node f fl fr <*> Node x l r = Node (f x) (fl <*> l) (fr <*> r)

-- Applicative::identity
-- pure id <*> v                       = v
--   = Node id (pure id) (pure id) <*> Leaf
--   = Leaf
--
--   = Node id (pure id) (pure id) <*> Node x l r
--   = Node (id x) (pure id <*> l) (pure id <*> r)
--   = Node x l r
------------------------------
-- newtype Const a b = Const { getConst :: a }

-- instance Monoid a => Applicative (Const a) where
  -- pure _              = Const mempty
  -- Const f <*> Const a = Const $ f `mappend` a

-- composition
-- pure (.) <*> u <*> v <*> w       =   u <*> (v <*> w)
--   = Const mempty <*> Const u <*> Const v <*> Const w
--   = Const u <*> Const v <*> Const w
--   =
------------------------------
-- data Tuple a b = Tuple a b
--                deriving (Show)

-- instance Monoid a => Applicative (Tuple a) where
--   pure                      = Tuple mempty
--   Tuple a b <*> Tuple a2 b2 = Tuple (a `mappend` a2) (b b2)

-- homomorphism
-- pure f <*> pure x              = pure (f x) = Tuple mempty (f x)
--    = Tuple mempty f <*> Tuple mempty x
--    = Tuple mempty (f x)
----------------------------------------------
-- instance Applicative Identity where
--   pure               = Identity
--   (<*>) (Identity f) = fmap f

-- instance Functor Identity where
--   fmap f (Identity x) = Identity $ f x

-- interchange
-- u' <*> pure y                  = pure ($ y) <*> u'
--   = Identity u <*> Identity y
--   = fmap u (Identity y)
--   = Identity $ u y

-- pure ($ y) <*> u'
--   = Identity ($ y) <*> Identity u
--   = fmap ($ y) (Identity u)
--   = fmap $ ($ y) u
--   = Identity $ u y
---------------------------------
-- data Tree a = Leaf
--             | Node a (Tree a) (Tree a)
--             deriving (Show)

-- instance Foldable Tree where
--   foldr _ z Leaf         = z
--   foldr f z (Node c l r) = foldr f (c `f` foldr f z r) l

-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldMap f = foldr (mappend . f) mempty

-- Foldable Law 1
-- fold           ≡ foldMap id == foldr (mappend . id) mempty == foldr mappend mempty
-- by definition (mcd)
----------------------------------------------
-- instance Foldable Identity where
--   foldr f b (Identity x) = f x b

-- instance Functor Identity where
--   fmap f (Identity x) = Identity $ f x

-- fold      = foldMap id
-- foldMap f = foldr (mappend . f) mempty

-- foldMap f x'             ≡ (fold . fmap f) x'
--   = foldr (mappend . f) mempty x'
--   = (mappend . f) x mempty
--   = f x `mappend` mempty

-- (fold . fmap f) x'
--   = (foldMap id . fmap f) x'
--   = foldMap id $ fmap f x'
--   = foldMap id $ Identity $ f x
--   = foldr mappend mempty $ Identity $ f x
--   = f x `mappend` mempty
----------------------------------
-- instance Traversable Identity where
--   traverse f (Identity x) = Identity <$> f x

-- naturality
-- t . traverse f          = traverse (t . f)    for every applicative transformation t
--    = (t . traverse f) x'
--    = t (Identity <$> f x)
--    = t (pure Identity <*> f x)
--    = t (pure Identity) <*> t (f x)
--    = pure Identity <*> t (f x)
--    = fmap Identity (t (f x))
--    = Identity <$> (t (f x))

-- traverse (t . f) x'
--   = Identity <$> (t . f) x
--   = Identity <$> t (f x)

-----------------------------------
-- instance Traversable Tree where
--   traverse _ Leaf         = pure Leaf
--   traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r

-- identity
-- traverse Identity x                    = Identity x
-- traverse Identity Leaf
--   = Identity Leaf

-- traverse Identity Node x l r           = Identity $ Node x l r
--   = Node <$> Identity x <*> traverse Identity l <*> traverse Identity r
--   = Identity $ Node x l r
-----------------------------------
-- instance Traversable (Tuple a) where
--   traverse f (Tuple a b) = Tuple a <$> f b

-- instance Functor (Tuple a) where
--   fmap f (Tuple a b) = Tuple a $ f b

 -- newtype Compose f g a = Compose (f (g a))

-- composition
-- traverse (Compose . fmap g . f)  t       = (Compose . fmap (traverse g) . traverse f) t
--    = Tuple a <$> (Compose . fmap g . f) b
--    = 
