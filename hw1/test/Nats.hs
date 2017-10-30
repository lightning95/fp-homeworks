module Nats
       ( Nat (..)
       , isEven
       , divNat
       , modNat
       , gcdNat
       ) where

data Nat = Z | S Nat 
    deriving (Show)

instance Eq Nat where
    (==) Z Z         = True
    (==) Z _         = False
    (==) _ Z         = False
    (==) (S x) (S y) = x == y

instance Ord Nat where
    (<=) Z _         = True
    (<=) _ Z         = False
    (<=) (S x) (S y) = x <= y

instance Num Nat where
    (+) = add
    (*) = mul
    (-) = sub
    abs = id

    signum x    = if x == Z then 0 else 1
    fromInteger = fromInt

fromInt :: Integer -> Nat
fromInt 0 = Z
fromInt x = S $ fromInt $ x - 1

add :: Nat -> Nat -> Nat
add Z x     = x
add (S x) y = S $ x `add` y

mul :: Nat -> Nat -> Nat
mul Z _     = Z
mul (S x) y = (x `mul` y) `add` y

sub :: Nat -> Nat -> Nat
sub x Z         = x
sub Z _         = error "Subtraction from zero"
sub (S x) (S y) = x `sub` y

----hard mode "ON"
isEven :: Nat -> Bool
isEven Z     = True
isEven (S x) = not $ isEven x
--isEven x = x `modNat` 2 == 0

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Division by zero"
divNat x y 
    | x < y     = Z 
    | otherwise = S $ (x `sub` y) `divNat` y

modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Division by zero"
modNat x y 
    | x < y     = x 
    | otherwise = (x `sub` y) `modNat` y
--modNat x y = x `sub` $ x `mul` $ x `divNat` y

gcdNat :: Nat -> Nat -> Nat
gcdNat x y 
    | x == Z || y == Z = x `add` y
    | x < y            = gcdNat (y `modNat` x) x 
    | otherwise        = gcdNat (x `modNat` y) y
