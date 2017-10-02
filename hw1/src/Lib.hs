module Lib
       (
       ) where

import           Data.Function  (on)
import           Data.Semigroup (Semigroup, (<>))

--block 4--"splitting"
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = foldr (\x t@(h:hs) -> if c == x then [] : t else (x : h) : hs) [[]]
 
joinWith :: a -> [[a]] -> [a]
joinWith c = foldr (\x z -> if null z then x else x ++ [c] ++ z) []
-----------------------------------------
--block 5
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []           = []
maybeConcat (Nothing:xs) = maybeConcat xs
maybeConcat (Just x :xs) = x ++ maybeConcat xs

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat []           = (mempty, mempty)
eitherConcat (Left  x:xs) = (x, mempty) `mappend` eitherConcat xs
eitherConcat (Right x:xs) = (mempty, x) `mappend` eitherConcat xs
----------------------------------------
data NonEmpty a = a :| [a]
    deriving (Show)

instance Semigroup (NonEmpty a) where  
    (a:|as) <> (b:|bs) = a :| (as <> [b] <> bs)
---------------------------------------
newtype Identity a = Identity { runIdentity :: a }
    deriving (Show)

--`blackbird` by Amar Shah
(...) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(...) = (.) . (.)

instance Semigroup a => Semigroup (Identity a) where
    (<>) = Identity ... (<>) `on` runIdentity

instance Monoid a => Monoid (Identity a) where
    mempty  = Identity mempty
    mappend = Identity ... mappend `on` runIdentity
-------------------------------------------
newtype Name = Name String
    deriving (Show)

instance Semigroup Name where
    (Name a) <> (Name b) = Name $ a <> "." <> b
    --(<>) = Name ... (\(Name a) (Name b) -> a <> "." <> b) 
    --(<>) = Name ... (\a b -> a <> "." <> b) `on` getName

instance Monoid Name where
    mempty  = Name []
    mappend = (<>)
---------------------------------------------
newtype Endo a = Endo { getEndo :: a -> a }
    
instance Semigroup (Endo a) where
    (<>) = Endo ... flip (.) `on` getEndo
    -- a <> b = Endo $ (getEndo b) . (getEndo a)

instance Monoid (Endo a) where
    mempty  = Endo id
    mappend = (<>)
-------------------------------------------
newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    -- <> :: Arrow a b -> Arrow a b -> Arrow a b
    -- a <> b = Arrow $ (getArrow a) <> (getArrow b)
    (<>) = Arrow ... (<>) `on` getArrow

instance Monoid b => Monoid (Arrow a b) where
    mempty  = Arrow $ const mempty
    mappend = Arrow ... mappend `on` getArrow
