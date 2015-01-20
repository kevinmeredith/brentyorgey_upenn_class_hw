-- http://www.cis.upenn.edu/~cis194/spring13
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- randomly create attackers and defenders, roll the die for maximum attackers & defenders
-- return a new BattleField per the battle results
instance Random Battlefield where
	random = first (\(as, ds) -> Battlefield as ds) . twoInts
	randomR = undefined

randomBF :: Rand StdGen Battlefield
randomBF = getRandom

twoInts :: RandomGen g => g -> ((Army, Army), g)
twoInts gen = let (one, gen')  = random gen
                  (two, gen'') = random gen'
              in ((abs one, abs two), gen'')

--fightSingleRound :: (Rand StdGen Battlefield) -> Battlefield
--fightSingleRound = 

getDie :: Int -> [DieValue]
getDie n 
  | n < 0     = []
  | otherwise = replicate n $ evalRand die (mkStdGen 55)
