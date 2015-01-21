-- http://www.cis.upenn.edu/~cis194/spring13
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Risk where

import Control.Monad.Random
import Data.List

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
--fightSingleRound rand_bf = 
--	where bf = evalRand rand_bf
--		  (as, ds) = (attackers bf, defenders bf)

type AttackersDice = [DieValue]
type DefendersDice = [DieValue]

type Deaths = (Army, Army)

attacker_dies :: (Int, Int)
attacker_dies = (1, 0)

defender_dies :: (Int, Int)
defender_dies = (0, 1)

compete :: AttackersDice -> DefendersDice -> Deaths
compete as_dice ds_dice = (attacker_deaths, defender_deaths)
    where attacker_deaths  = foldr (\x acc -> fst x + acc) 0 results
          defender_deaths  = foldr (\x acc -> snd x + acc) 0 results
          results          = map (\(x, y) -> if x > y then defender_dies else attacker_dies) $ zip as ds -- defender wins on EQ or GT
          as               = reverse . sort . map (unDV) $ as_dice
          ds               = reverse . sort . map (unDV) $ ds_dice

rollDieN :: Int -> [DieValue]
rollDieN n 
  | n < 0     = []
  | otherwise = evalRand die (mkStdGen n) : rollDieN (n-1) 

-- if the Attacker rolls a 1, 2, and 3 & the Defender rolls a 2 and 3, then
-- the Attacker should lose 2 soldiers, and the Defender none.
competeTest :: Bool
competeTest = (compete as_dice ds_dice) == expected
  where
  	expected = (2, 0)
  	as_dice  = [DV 1, DV 2, DV 3]
  	ds_dice  = [DV 2, DV 3]
