module Lib
       ( plusTwo
       ) where

import Data.List

plusTwo :: [Int] -> [Int]
plusTwo = map (+2)


-----
-- block 1
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let [x, y, z] = sort [a, b, c]
                   in (x, y, z)

