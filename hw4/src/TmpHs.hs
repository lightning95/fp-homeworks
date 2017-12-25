{-# LANGUAGE TemplateHaskell #-}

module TmpHs
  ( chooseByIndices
  ) where

import           Language.Haskell.TH (Exp, Name, Q, lamE, mkName, tupE, tupP,
                                      varE, varP)
import           ShowT               (mkShowT)

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices sz is = do
  let names :: [Name]
      names = map (mkName . (\x -> "x" ++ show x)) (take sz ([0..] :: [Int]))
  lamE [tupP $ map varP names] (tupE $ map (\x -> varE $ names !! x) is)

data MyData = MyData
     { foo :: String
     , bar :: Int
     } deriving (Show)

mkShowT ''MyData

data MD = MD
     { lol      :: [String]
     , kek      :: Integer
     , cheburek :: Float
     } deriving (Show)

mkShowT ''MD
