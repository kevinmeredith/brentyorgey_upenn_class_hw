-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)

-- Problems 1-4

import Data.Char

-- credits: http://stackoverflow.com/a/2838888/409976
--          http://stackoverflow.com/q/4061777/409976
--1234 -> [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits x 
  | x <= 0    = []
  | otherwise = (map (toInteger . digitToInt) . show) x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- double every other number (from right to left)
-- f [1,2,3] == [1,4,3]
-- f [8,7,6,5] == [16,7,12,5]
--   reversed = [5,6,7,8], then double each odd element, and then reverse again
--              [5,12,7,16], then reverse -> [16,7,12,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = (reverse . (map (\(x, y) -> if(odd y) then (x*2) else x))) $ zip (reverse xs) [0..]

-- calculate the sum of all digits
-- example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits = (sum . concat. map (toDigits))

validate :: Integer -> Bool
validate = ((== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits)

-- Problem 5 (towers of Hanoi)

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = hanoi' n a b c n []
                  where hanoi' m x y z total acc 
                         | m  == (-1)                 = reverse acc
                         | ((total - m) `mod` 3) == 0 = hanoi' (m-1) x y z total ((x,z) : acc)
                         | ((total - m) `mod` 3) == 1 = hanoi' (m-1) x y z total ((x,y) : acc)
                         | ((total - m) `mod` 3) == 2 = hanoi' (m-1) x y z total ((z,y) : acc)
                  
