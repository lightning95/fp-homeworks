{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Monsters
       ( Hero (..)
       , Monster (..) 
       , gloriousBattle
       , isAlive
       ) where

import           Data.Semigroup (Semigroup, (<>))
import           Prelude        hiding (log, round)

data Stats = Stats
    { hp     :: Int
    , attack :: Int
    -- , defence :: Int
    } deriving (Show)

newtype Hero = Hero { getStats :: Stats }
    deriving (Show)
          
newtype Monster = Monster { info :: Stats }
    deriving (Show)  

class Unit a where
    isAlive   :: a -> Bool 
    getHP     :: a -> Int
    getAttack :: a -> Int

instance Unit Hero where
    isAlive   = (0 < ) . getHP
    getHP     = hp . getStats
    getAttack = attack . getStats

instance Unit Monster where
    isAlive   = (0 < ) . getHP
    getHP     = hp . info
    getAttack = attack . info

data Result = Result 
    { hero    :: Hero
    , monster :: Monster
    , log     :: Log
    } deriving (Show)

instance Semigroup Result where
    (<>) a (Result h m lg) = Result h m $ log a <> lg

newtype Log = Log [String] 
    deriving (Show, Semigroup)    

gloriousBattle :: Hero -> Monster -> Result
gloriousBattle hh mm = 
    let (res, end) = round hh mm
    in if end then res
    else res <> gloriousBattle (hero res) (monster res)
    where   
      round :: Hero -> Monster -> (Result, Bool)
      round h m = 
        let mA  = getAttack m
            hA  = getAttack h 
            mHP = getHP m - hA
            hHP = getHP h - mA
        in if mHP <= 0 then 
          (Result h (Monster (Stats 0 mA)) $ gloriousWin hA, True)                                     
        else if hHP <= 0 then
          (Result (Hero (Stats 0 hA)) (Monster (Stats mHP mA)) $ heroicDefeat hA mA, True)
        else
          (Result (Hero (Stats hHP hA)) (Monster (Stats mHP mA)) $ comments hA mA, False)

comments :: Int -> Int -> Log
comments hA mA = Log $ ["Hero hits for " ++ show hA ++ " and receives "] 
    <> [show mA ++ " dmg. Battle continues...\n"]

gloriousWin :: Int -> Log
gloriousWin a = Log $ ["Hero hits for " ++ show a ++ " and defeats the beast! "] 
    <> ["Welcome the winner!\n"]
      
heroicDefeat :: Int -> Int -> Log
heroicDefeat hA mA = Log $ ["Hero hits for " ++ show hA ++ ", but receives "] 
    <> ["a fatal blow of " ++ show mA ++ " dmg and dies in agony. "] 
    <> ["The beast remains undefeated!\n"]
