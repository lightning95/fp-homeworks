{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block1And2
       ( order3
       , highestBit
       , highestBit'
       , smartReplicate
       , contains
       , removeAt
       , removeAt'
       , collectEvery
       , stringSum
       , stringSum'
       , mergeSort
       ) where

import           Data.Char     (isDigit)
import           Data.Function (fix, on)
import           Data.List     (elem, sort)
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen
--------------------------------------------------------------------
-- block 1
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (\[x, y, z] -> (x, y, z)) $ sort [a, b, c]

highestBit :: Int -> Int
highestBit x = if x < 1 then error "argument < 1" else 
    fix (\f c -> if c * 2 > x then c else f (c * 2)) 1


highestBit' :: Int -> (Int, Int)               
highestBit' x = if x < 1 then error "argument < 1" else
    fix (\f c st -> if c * 2 > x then (c, st) else f (c * 2) (st + 1)) 1 0

smartReplicate :: [Int] -> [Int]
smartReplicate = concat . map (\x -> replicate x x)
--smartReplicate = concat . map (\x -> stimes x [x]) import Data.Semigroup (Semigroup)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem
--------------------------------------------------------------------
--block 2
removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 0 (_:xs) = xs
removeAt i (x:xs) = x : removeAt (i - 1) xs

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ []     = (Nothing, [])
removeAt' 0 (x:xs) = (Just x, xs)
removeAt' i (x:xs) = (\(mb, ls) -> (mb, x : ls)) $ removeAt' (i - 1) xs                     

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k xs = case splitAt (k - 1) xs of
                        t@(y, []) -> t
                        (y, h:hs) -> (y, [h]) `mappend` (collectEvery k hs)

stringSum :: String -> Int
stringSum = sum . map read . words

stringSum' :: String -> Int
stringSum' = let rm :: String -> String
                 rm ('+':x) = if isDigit $ head x then x else error "+notDigit"
                 rm s       = s
             in sum . map (read . rm) . words

mergeSort :: forall a . Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort s   = let (l, r) = splitAt (length s `div` 2) s
                in (merge `on` mergeSort) l r
                where 
                  merge :: [a] -> [a] -> [a]
                  merge [] x = x
                  merge x [] = x
                  merge l@(a:as) r@(b:bs) 
                      | a < b     = a : merge as r 
                      | otherwise = b : merge l bs
