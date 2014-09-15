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
-- Given the number of discs and names for the three pegs, `hanoi`
--   should return a list of moves to be performed to move the stack of
--   discs from the first peg to the second.
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> Maybe [Move]
hanoi n a b c
 | n < 2     = Nothing -- TODO fix!
 | n == 2    = Just $ (a,c) : (a,b) : (c,b) : []
 | odd n     = Just $ (a,b) : (a,c) : pickBest a b c (mkStack (n-2), [1], [2])
 | otherwise = Just $ (a,c) : (a,b) : pickBest a b c (mkStack (n-2), [2], [1])

pickBestBGoal :: -> Peg -> Peg -> Peg -> ([Int], [Int], [Int]) -> Move
pickBestBGoal a b c (x:xs, y:ys:, z:zs) = if (y > z && 
pickBestBGoal a b c (x:xs, y:ys:, z:zs) = 
pickBestBGoal a b c (x:xs, y:ys:, z:zs) = 

mkStack :: Int -> [Int]
mkStack n = [3..n]