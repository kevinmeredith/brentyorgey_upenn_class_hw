-- http://www.cis.upenn.edu/~cis194/spring13
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- randomly create attackers and defenders, roll the die for maximum attackers & defenders
---- return a new BattleField per the battle results
instance Random Battlefield where
	random = fightSingleRound die (Battlefield { attackers = rand1, defenders = rand2 })
	   where (rand1, rand2) = twoInts 666
	randomR = undefined


twoInts :: Int -> (Int, Int)
twoInts x = let (one, gen) = random (mkStdGen x)
                (two, _)   = random gen 
            in (abs one, abs two)

fightSingleRound :: (Rand StdGen DieValue) -> Battlefield -> Battlefield
fightSingleRound = undefined