module TestB1And2
    (
    ) where

import           Block1And2    (order3
                               , highestBit
                               , highestBit'
                               , smartReplicate
                               , contains
                               , removeAt
                               , removeAt'
                               , collectEvery
                               , stringSum
                               , stringSum'
                               , mergeSort)        
import           Control.Monad (replicateM_)
import           System.Random (newStdGen, randomRs)

main :: IO ()
main = do
    putStr "order3 (5, 2, 10) -> "
    print orderTest
    
    putStr "highestBit 15 -> "   
    print highestBitTest1

    putStr "highestBit 16 -> "   
    print highestBitTest2

    putStr "highestBit 1 -> "   
    print highestBitTest3

    putStr "highestBit' 15 -> "   
    print highestBitTest'1

    putStr "highestBit' 16 -> "   
    print highestBitTest'2

    putStr "highestBit' 1 -> "   
    print highestBitTest'3

    putStr "smartReplicate [1,2,3,0,-1] -> "
    print smartReplicateTest

    putStr "contains 3 [[1..5], [2,0], [3,4]] -> "
    print containsTest

    putStrLn "\nblock2"

    putStr "removeAt 1 [1,2,3] -> "
    print removeAtTest1

    putStr "removeAt 10 [1,2,3] -> "
    print removeAtTest2

    putStr "removeAt 3 [1..5] -> "
    print removeAtTest3

    putStr "removeAt 2 \"abc\" -> "
    print removeAtTest4

    
    putStr "removeAt' 1 [1,2,3] -> "
    print removeAtTest'1

    putStr "removeAt' 10 [1,2,3] -> "
    print removeAtTest'2

    putStr "removeAt' 3 [1..5] -> "
    print removeAtTest'3

    putStr "removeAt' 2 \"abc\" -> "
    print removeAtTest'4
    

    putStr "collectEvery 3 [1..8] ->"
    print collectEveryTest

    putStrLn "stringSum tests:"
    mapM_ (\x ->putStrLn (x ++ " = " ++ show (stringSum x))) passTests
    
    putStrLn "stringSum' tests:"
    mapM_ (\x ->putStrLn (x ++ " = " ++ show (stringSum' x))) advancedTests

    putStrLn "mergeSort tests: "
    print $ mergeSort [2, 1, 0, 3, 10, 5]

    let f :: IO ()
        f = do
        example <- randomIntList 5 (-10) 10
        putStrLn $ "random: " ++ show example ++ ", sorted: " ++ show (mergeSort example)

    replicateM_ 5 f
--------------------------------------------------------------------
-- block 1
orderTest :: (Int, Int, Int)
orderTest = order3 (5, 2, 10)

highestBitTest1 :: Int
highestBitTest1 = highestBit 15
highestBitTest2 :: Int
highestBitTest2 = highestBit 16
highestBitTest3 :: Int
highestBitTest3 = highestBit 1

highestBitTest'1 :: (Int, Int)
highestBitTest'1 = highestBit' 15
highestBitTest'2 :: (Int, Int)
highestBitTest'2 = highestBit' 16
highestBitTest'3 :: (Int, Int)
highestBitTest'3 = highestBit' 1

smartReplicateTest :: [Int]
smartReplicateTest = smartReplicate [1,2,3, 0, -1]

containsTest :: [[Int]]
containsTest = contains 3 [[1..5], [2,0], [3,4]]
------------------------------------------------
--block2
removeAtTest1 :: [Int]
removeAtTest1 = removeAt 1 [1,2,3]

removeAtTest2 :: [Int]
removeAtTest2 = removeAt 10 [1,2,3]

removeAtTest3 :: [Int]
removeAtTest3 = removeAt 3 [1..5]

removeAtTest4 :: String
removeAtTest4 = removeAt 2 "abc"


removeAtTest'1 :: (Maybe Int, [Int])
removeAtTest'1 = removeAt' 1 [1,2,3]

removeAtTest'2 :: (Maybe Int, [Int])
removeAtTest'2 = removeAt' 10 [1,2,3]

removeAtTest'3 :: (Maybe Int, [Int])
removeAtTest'3 = removeAt' 3 [1..5]

removeAtTest'4 :: (Maybe Char, String)
removeAtTest'4 = removeAt' 2 "abc"

collectEveryTest :: ([Int], [Int])
collectEveryTest = collectEvery 3 [1..8]
-----------------------------------------------
passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
            , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
            ]

mustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]
advancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1"]
----------------------------------------------
randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen
