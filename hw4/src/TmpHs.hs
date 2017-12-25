module TmpHs
  ( chooseByIndices
  ) where

import           Language.Haskell.TH (Exp, Name, Q, lamE, mkName, tupE, tupP,
                                      varE, varP)

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices sz is = do
  let names :: [Name]
      names = map (mkName . (\x -> "x" ++ show x)) (take sz ([0..] :: [Int]))
  lamE [tupP $ map varP names] (tupE $ map (\x -> varE $ names !! x) is)
