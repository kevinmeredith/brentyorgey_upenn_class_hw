-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)

-- Problem 1

-- TODO!
--1234 -> [1,2,3,4]
--toDigits :: Integer -> [Integer]
--toDigits x 
--  | x <= 0    = []
--  | otherwise = []

-- multiply every other (beginning at first element) by 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = map (\(x, y) -> if(even y) then (x*2) else x) $ zip xs [0..]

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate = Integer -> Bool
validate = ((`mod` 10) . sum . doubleEveryOther . toDigits)

-- Problem 2