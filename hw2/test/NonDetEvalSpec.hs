module NonDetEvalSpec (main, spec) where

import           Control.Monad  (replicateM_)
import qualified Data.List      as DL (permutations)
import           OptionalMonads (bin, combinations, permutations)
import           System.Random  (newStdGen, randomRs)
import           Test.Hspec


spec :: Spec
spec = do
  it "bin" $ do
    bin 0 `shouldBe` [ [] ]
    bin 1 `shouldMatchList` [ [ 0 ], [ 1 ] ]
    bin 2 `shouldMatchList` [ [ 0 , 0 ] , [ 0 , 1 ] , [ 1 , 0 ] , [ 1 , 1 ] ]
    bin 3 `shouldMatchList` bin3
    bin 4 `shouldMatchList` bin4
----------------------------------
  it "combinations" $ do
    combinations 4 2 `shouldMatchList` [ [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4] ]
    combinations 1 1 `shouldBe` [ [ 1 ] ]
    combinations 5 3 `shouldMatchList` comb53
    combinations 7 3 `shouldMatchList` comb73
----------------------------------
  it "permutations" $ do
    permutations [ 22, 10, 5 ] `shouldMatchList` perm3
    let gen :: IO ()
        gen = do
            test <- randomIntList 6 (-100) 100
            permutations test `shouldMatchList` DL.permutations test
    replicateM_ 100 gen


main :: IO ()
main = hspec spec
------------------------------
randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen
-------------------------------
bin3 :: [[Int]]
bin3 = [ [ 0 , 0 , 0 ], [ 0 , 0 , 1 ], [ 0 , 1 , 0 ], [ 0 , 1 , 1 ],
         [ 1 , 0 , 0 ], [ 1 , 0 , 1 ], [ 1 , 1 , 0 ], [ 1 , 1 , 1 ] ]
bin4 :: [ [Int] ]
bin4 = [ [ 0, 0, 0, 0], [ 0, 0, 0, 1], [ 0, 0, 1, 0], [ 0, 0, 1, 1],
         [ 0, 1, 0, 0], [ 0, 1, 0, 1], [ 0, 1, 1, 0], [ 0, 1, 1, 1],
         [ 1, 0, 0, 0], [ 1, 0, 0, 1], [ 1, 0, 1, 0], [ 1, 0, 1, 1],
         [ 1, 1, 0, 0], [ 1, 1, 0, 1], [ 1, 1, 1, 0], [ 1, 1, 1, 1] ]
--------------------------------
comb53 :: [[Int]]
comb53 = [ [1,2,3], [1,2,4], [1,2,5],
           [1,3,4], [1,3,5], [1,4,5],
           [2,3,4], [2,3,5], [2,4,5],
           [3,4,5] ]
---------------------------------
comb73 :: [ [ Int ] ]
comb73 = [ [1,2,3],[1,2,4],[1,2,5],[1,2,6],[1,2,7],
           [1,3,4],[1,3,5],[1,3,6],[1,3,7],
           [1,4,5],[1,4,6],[1,4,7],
           [1,5,6],[1,5,7],[1,6,7],
           [2,3,4],[2,3,5],[2,3,6],[2,3,7],[2,4,5],[2,4,6],[2,4,7],
           [2,5,6],[2,5,7],[2,6,7],
           [3,4,5],[3,4,6],[3,4,7],[3,5,6],[3,5,7],[3,6,7],
           [4,5,6],[4,5,7],[4,6,7],[5,6,7] ]
-----------------------------------
perm3 :: [ [ Int ] ]
perm3 = [ [22, 10, 5], [22, 5, 10],
          [10, 22, 5], [10, 5, 22],
          [5, 22, 10], [5, 10, 22] ]
