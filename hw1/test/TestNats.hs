module TestNats
    (
    ) where

import           Nats ( Nat (..)
                      , isEven
                      , divNat
                      , modNat
                      , gcdNat 
                      )        

main :: IO ()
main = do
    let zero = Z
        one  = S Z
        two  = one + one
    
    putStr "1 + 1 -> "
    print $ one + one

    putStr "2 * (2 + 1) -> "
    print $ two * (two + 1)

    putStr "(2 + 1) - 2 -> "
    print $ two + one - two

    putStr "|2| -> "
    print $ abs two

    putStr "signum 2 -> "
    print $ signum two 

    putStr "signum 0 -> "
    print $ signum zero

    putStr "fromInteger 2 -> "
    print $ fromInteger 2

    putStr "isEven 2 -> "
    print $ isEven two

    putStr "isEven (2 + 1) -> "
    print $ isEven $ two + one

    putStr "5 // 2 -> "
    print $ (two * two + one) `divNat` two

    putStr "5 % 3 -> "
    print $ (two * two + one) `modNat` (two + one)

    putStr "5 - 3 -> "
    print $ (two * two + one) - (two + one)

    putStr "5 - 2 == 3 -> "
    print $ (two * two + one - two) == (two + one)

    let five  = two + three
        three = two + one
    
    putStr "5 * 3 `gcd` 5 * 5 -> "
    print $ (five * three) `gcdNat` (five * five)
    
