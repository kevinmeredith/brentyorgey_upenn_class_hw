-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)

{-# OPTIONS_GHC -Wall #-}

import Data.Set (empty, insert, toList)
-- rewrite the following as "Wholemeal" programs
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) 
 | even x     = (x - 2) * fun1 xs
 | otherwise  = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (product . map (\x -> x-2) . filter (even))

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = (sum . f)

f :: Integer -> [Integer]
f x 
 | x == 1    = []
 | even x    = x : f (x `div` 2)
 | otherwise = f (3*x + 1) 

-- Given a list of Booleans, return true so long as there's an odd number (1,3,5,...)
-- True elements
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x == True then not acc else acc) False

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram  n = (map (\x -> 2*x + 1). filter (excluded) . filter (odd)) [3..(2*n+2)]
     where excluded     = (\x -> not $ x `elem` bad)
     	   bad          = (numsToSet . numsToRemove) n
    
numsToRemove :: Integer -> [(Integer, Integer)]
numsToRemove n = [ (i, j) | i <- [2..n], j <- [2..n], i <= j, i * j + 2*i*j < n]

-- http://stackoverflow.com/a/18627837/409976
numsToSet :: [(Integer, Integer)] -> [Integer]
numsToSet = (toList . (foldr (\(x,y) acc -> insert x $ insert y acc) empty))