module DaysAndVectors
       (
       ) where

import           Data.Function (on)
import           Data.Ix       (Ix, inRange, index)

--block 3
data DayOfWeek = Mo | Tu | We | Th | Fr | Sa | Su 
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Ix)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Su = Mo
nextDay x  = succ x

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays d x = iterate nextDay d !! (x `mod` 7)

isWeekend :: DayOfWeek -> Bool
isWeekend = inRange (Sa, Su) 
 
daysToParty :: DayOfWeek -> Int
daysToParty x 
    | isWeekend x = 6 - index (Sa, Su) x 
    | otherwise   = 4 - index (Mo, Fr) x
--------------------------------
data Vector a = Vector2D a a 
              | Vector3D a a a
              deriving (Eq, Show)

packVector :: Vector a -> [a]
packVector (Vector2D x y)   = [x, y]
packVector (Vector3D x y z) = [x, y, z]

unpackVector :: [a] -> Vector a
unpackVector [x, y]    = Vector2D x y
unpackVector [x, y, z] = Vector3D x y z
unpackVector _         = error "not 2 or 3 dimensions"

getLength :: (Floating a, Num a) => Vector a -> a
getLength = sqrt . sum . map (^2) . packVector

zipWith' :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f a@[_, _] b@[_, _, _] = zipWith f (a ++ [0]) b
zipWith' f a@[_, _, _] b@[_, _] = zipWith f a $ b ++ [0]
zipWith' f a           b        = zipWith f a b

--`blackbird` by Amar Shah
(...) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(...) = (.) . (.)

addV :: Num a => Vector a -> Vector a -> Vector a
addV = unpackVector ... zipWith' (+) `on` packVector

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct = sum ... zipWith' (*) `on` packVector

negateV :: Num a => Vector a -> Vector a
negateV = unpackVector . map negate . packVector

distance :: Floating a => Vector a -> Vector a -> a
distance a b = getLength . addV a $ negateV b

crossProduct :: Num a => Vector a -> Vector a -> Vector a
crossProduct (Vector3D x y z) (Vector3D x' y' z') = 
    Vector3D (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')
crossProduct a                b                   = 
    (crossProduct `on` to3D) a b
    where 
      to3D :: Num a => Vector a -> Vector a
      to3D (Vector2D x y) = Vector3D x y 0
      to3D x              = x            
