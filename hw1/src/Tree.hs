module Tree
       ( Tree (..)
       , empty
       , size
       , find
       , insert
       , fromList
       , toList 
       ) where

import           Data.Semigroup (Semigroup, (<>))

data Tree a = Leaf 
            | Node a (Tree a) (Tree a) 
            deriving (Show)

empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

find :: Ord a => a -> Tree a -> Bool
find _ Leaf         = False
find x (Node c l r) = case compare x c of
                          LT -> find x l
                          EQ -> True
                          GT -> find x r        

insert :: Ord a => a -> Tree a -> Tree a                           
insert x Leaf           = Node x Leaf Leaf
insert x t@(Node c l r) = case compare x c of
                              LT -> Node c (insert x l) r
                              EQ -> t
                              GT -> Node c l (insert x r)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf


instance Ord a => Semigroup (Tree a) where
    (<>) = foldr insert


instance Ord a => Monoid (Tree a) where
    mempty  = Leaf
    mappend = foldr insert


instance Foldable Tree where
    -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
    foldMap _ Leaf         = mempty
    foldMap f (Node c l r) = foldMap f l `mappend`
                             f c         `mappend`
                             foldMap f r
    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    foldr _ z Leaf         = z
    foldr f z (Node c l r) = foldr f (c `f` foldr f z r) l

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node c l r) = toList l ++ [c] ++ toList r
