{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either        (rights)
import           Data.Text          (Text, pack, splitOn, unpack)
import           InterpreterIO      (interpretIO)
import           System.Environment (getArgs)
import           Text.Megaparsec    (parse)
import           VarsParser         (Assign, assignParser)

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let
    strs :: [Text]
    strs = splitOn "\n" $ pack file
    unpacked :: [String]
    unpacked = filter (not . null) $ map unpack strs
    linez :: [Assign]
    linez = rights $ map (parse assignParser "") unpacked
  res <- interpretIO linez
  case res of
    Left e       -> print e
    Right (_, b) -> print b
