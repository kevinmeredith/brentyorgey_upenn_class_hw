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

instance Random Battlefield where
	random            = first (\(as, ds) -> Battlefield as ds) . twoInts
	randomR (bf1, bf2) = first (\(as, ds) -> Battlefield as ds) . genInt . randomR (max 1 (attackers bf1), max 20 (defenders bf2))

genInt :: RandomGen g => (Army, g) -> ((Army, Army), g)
genInt (x, gen) = ( (x, i), gen)
  where
   i = (`mod` 25) . fst $ random $ mkStdGen x 

randomBF :: Rand StdGen Battlefield
randomBF = getRandom

twoInts :: RandomGen g => g -> ((Army, Army), g)
twoInts gen = let (one, gen')  = random gen
                  (two, gen'') = random gen'
              in (( (`mod` 100) . abs $ one, (`mod` 100) . abs $ two), gen'') -- using 10 to make the numbers smaller

fightSingleRound :: RandomGen g => g -> Battlefield
fightSingleRound gen = updatePerBattle bf (compete a_dice d_dice)
	where
    bf     = evalRand getRandom $ gen
    a_dice = rollDieN . getLegalAttackers $ (attackers bf)
    d_dice = rollDieN . getLegalDefenders $ (defenders bf)

-- defender must have at least 2 left at base
-- TODO: make Maybe?
getLegalDefenders :: Army -> Army
getLegalDefenders n 
  | n >= 4    = 2
  | n == 3    = 1
  | otherwise = 0

-- attackers must have at least 1 left at base
getLegalAttackers :: Army -> Army
getLegalAttackers n 
  | n >= 4    = 3
  | n == 3    = 2
  | n == 2    = 1
  | otherwise = 0

updatePerBattle :: Battlefield -> Deaths -> Battlefield
updatePerBattle (Battlefield as ds) (a_deaths, d_deaths) = Battlefield (as - a_deaths) (ds - d_deaths)

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

-- testing

-- if the Attacker rolls a 1, 2, and 3 & the Defender rolls a 2 and 3, then
-- the Attacker should lose 2 soldiers, and the Defender none.
competeTest :: Bool
competeTest = (compete as_dice ds_dice) == expected
  where
  	expected = (2, 0)
  	as_dice  = [DV 1, DV 2, DV 3]
  	ds_dice  = [DV 2, DV 3]

competeTest2 :: Bool
competeTest2 = (compete as_dice ds_dice) == expected
  where
    expected = (0, 2)
    as_dice  = [DV 1, DV 100, DV 555]
    ds_dice  = [DV 2, DV 3]

competeTest3 :: Bool
competeTest3 = (compete as_dice ds_dice) == expected
  where
    expected = (1, 1)
    as_dice  = [DV 1, DV 100, DV 555]
    ds_dice  = [DV 2, DV 99999]

getLegalAttackersTests :: Bool
getLegalAttackersTests = all (== True) [getLegalAsTest1, getLegalAsTest2, getLegalAsTest3, getLegalAsTest4, getLegalAsTest5]

getLegalAsTest1 :: Bool
getLegalAsTest1 = (getLegalAttackers 3) == 2

getLegalAsTest2 :: Bool
getLegalAsTest2 = (getLegalAttackers 4) == 3

getLegalAsTest3 :: Bool
getLegalAsTest3 = (getLegalAttackers 9999) == 3

getLegalAsTest4 :: Bool
getLegalAsTest4 = (getLegalAttackers 2) == 1

getLegalAsTest5 :: Bool
getLegalAsTest5 = (getLegalAttackers 1) == 0

getLegalDefendersTests :: Bool
getLegalDefendersTests = all (== True) [getLegalDsTest1, getLegalDsTest2, getLegalDsTest3, getLegalDsTest4,getLegalDsTest5, getLegalDsTest6, getLegalDsTest7]

getLegalDsTest1 :: Bool
getLegalDsTest1 = (getLegalDefenders 2) == 0

getLegalDsTest2 :: Bool
getLegalDsTest2 = (getLegalDefenders 1) == 0

getLegalDsTest3 :: Bool
getLegalDsTest3 = (getLegalDefenders 0) == 0

getLegalDsTest4 :: Bool
getLegalDsTest4 = (getLegalDefenders 3) == 1

getLegalDsTest5 :: Bool
getLegalDsTest5 = (getLegalDefenders 4) == 2

getLegalDsTest6 :: Bool
getLegalDsTest6 = (getLegalDefenders 5) == 2

getLegalDsTest7 :: Bool
getLegalDsTest7 = (getLegalDefenders 765) == 2