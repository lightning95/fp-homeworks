module TestDaysVectors
    (
    ) where

import           DaysAndVectors ( DayOfWeek (..)
                                , nextDay
                                , afterDays
                                , isWeekend
                                , daysToParty
                                , Vector (..)
                                , getLength
                                , addV
                                , dotProduct
                                , distance
                                , crossProduct
                                )

main :: IO ()
main = do
    putStr "nextDay Mo -> "
    print nextDayTest1

    putStr "nextDay Su -> "
    print nextDayTest2

    putStrLn ""

    putStr "afterDays Mo 1 -> "
    print afterDaysTest1

    putStr "afterDays Mo 100 -> "
    print afterDaysTest2

    putStr "afterDays Mo (-100) -> "
    print afterDaysTest3

    putStrLn ""

    putStr "isWeekend Su -> "
    print isWeekendTest1

    putStr "isWeekend Sa -> "
    print isWeekendTest2

    putStr "isWeekend Mo -> "
    print isWeekendTest3

    putStrLn ""

    putStr "daysToParty Fr -> "
    print daysToPartyTest1

    putStr "daysToParty Sa -> "
    print daysToPartyTest2

    putStr "daysToParty Mo -> "
    print daysToPartyTest3

    putStrLn "\nVectors"

    putStr "(1, 1) + (0, 0) -> "
    print $ (Vector2D 1 1) `addV` Vector2D 0 0

    putStr "|(1.0, 1.0)| -> "
    print $ getLength $ Vector2D 1.0 1.0

    putStr "(1, 2) * (3, 4, 5) -> "
    print $ (Vector2D 1 2) `dotProduct` (Vector3D 3 4 5)

    putStr "(3, 5) `distance` (4, 6) -> "
    print $ (Vector2D 3 5) `distance` (Vector2D 4 6)


    putStr "(1, 2, 3) x (1, 2, 3) -> "
    print $ (Vector3D 1 2 3) `crossProduct` (Vector3D 1 2 3)
------------------
nextDayTest1 :: DayOfWeek    
nextDayTest1 = nextDay Mo

nextDayTest2 :: DayOfWeek    
nextDayTest2 = nextDay Su


afterDaysTest1 :: DayOfWeek
afterDaysTest1 = afterDays Mo 1

afterDaysTest2 :: DayOfWeek
afterDaysTest2 = afterDays Mo 100

afterDaysTest3 :: DayOfWeek
afterDaysTest3 = afterDays Mo (-100)


isWeekendTest1 :: Bool
isWeekendTest1 = isWeekend Su

isWeekendTest2 :: Bool
isWeekendTest2 = isWeekend Sa

isWeekendTest3 :: Bool
isWeekendTest3 = isWeekend Mo


daysToPartyTest1 :: Int
daysToPartyTest1 = daysToParty Fr 

daysToPartyTest2 :: Int
daysToPartyTest2 = daysToParty Sa 

daysToPartyTest3 :: Int
daysToPartyTest3 = daysToParty Mo
------------------------------


