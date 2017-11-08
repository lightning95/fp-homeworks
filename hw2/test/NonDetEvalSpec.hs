module NonDetEvalSpec (main, spec) where

import           Test.Hspec

import           OptionalMonads(bin, permutations, combinations)


spec :: Spec
spec = do
  it "bin" $ do
    bin 1 `shouldMatchList` [ [ 0 ], [ 1 ] ]
    bin 2 `shouldMatchList` [ [ 0 , 0 ] , [ 0 , 1 ] , [ 1 , 0 ] , [ 1 , 1 ] ]
    bin 3 `shouldMatchList` bin3
  it "permutations" $ do
    permutations [ 22, 10, 5 ] `shouldMatchList` perm3
    permutations [ "22" ] `shouldBe` [ [ "22" ] ]
  it "combinations" $ do
    combinations 4 2 `shouldMatchList` [ [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4] ]
    combinations 1 1 `shouldBe` [ [ 1 ] ]

bin3 :: [[Int]]
bin3 = [ [ 0 , 0 , 0 ],
         [ 0 , 0 , 1 ],
         [ 0 , 1 , 0 ],
         [ 0 , 1 , 1 ],
         [ 1 , 0 , 0 ],
         [ 1 , 0 , 1 ],
         [ 1 , 1 , 0 ],
         [ 1 , 1 , 1 ] ]

perm3 :: [ [ Int ] ]
perm3 = [ [22, 10, 5],
          [22, 5, 10],
          [10, 22, 5],
          [10, 5, 22],
          [5, 22, 10],
          [5, 10, 22] ]

main :: IO ()
main = hspec spec
