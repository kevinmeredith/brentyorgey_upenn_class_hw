-- If the integers from 1 to 999,999,999 are written as words, 
-- sorted alphabetically, and concatenated, what is the 51 billionth letter?
-- http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers1/

-- TODO: better than `Integer` type for 0 to 9?

--ghci> x
--901 555 333
--ghci> x `div` 1000
--901555
--ghci> x `div` 1000 `mod` 1000
--555
--ghci> x `mod` 1000
--333

-- NOTE: wrong to assume that there will be 9 digits present

import Data.Monoid
import Data.List

fiftyOneBillion :: Integer
fiftyOneBillion = 51000000000

findFiftyOneBillion :: Char
findFiftyOneBillion = findCharAtIndex fiftyOneBillion

-- FIXME : don't use head
findCharAtIndex :: Integer -> Char
findCharAtIndex idx = head . drop' (idx - 1) . concat . sort . map integerToWord $ [1..999999999]

drop' :: Integer -> [a] -> [a]
drop' n xxs@(_:xs) 
 | n <= 0    = xxs
 | null xs   = []
 | otherwise = drop' (n-1) xs

integerToWord :: Integer -> String
integerToWord x = (millions x) `mappend` rest
  where mills = millions x
        thous = thousands x
        hunds = hundreds x
        rest = if ((not . null $ thous) && (not . null $ hunds)) then thous `mappend` "and" `mappend` hunds
        	   else thous `mappend` hunds

-- TODO: use bit shift instead?
-- TODO: bad to use `div`? Did not figure out Fractional -> Integer
millions :: Integer -> String
millions x 
 | null result = mempty 
 | otherwise   = result `mappend` "million"
 where result = hundredsToWord (x `div` 1000000)

thousands :: Integer -> String
thousands x 
 | null result = mempty
 | otherwise   = result `mappend` "thousand"
 where result = hundredsToWord (x `mod` 1000000 `div` 1000)

hundreds :: Integer -> String
hundreds x 
 | null result = mempty
 | otherwise   = result 
 where result = hundredsToWord (x `mod` 1000)

hundredsToWord :: Integer -> String
hundredsToWord x  
 | x > 99    = onesToWord hundredsPlace `mappend` "hundred" `mappend` tensDigitToWord tensPlace onesPlace
 | x > 9     = tensDigitToWord tensPlace onesPlace
 | otherwise = onesToWord x
     where hundredsPlace = x `div` 100
     	   tensPlace     = x `mod` 100 `div` 10
           onesPlace     = x `mod` 10

tensDigitToWord :: Integer -> Integer -> String
tensDigitToWord x y
 | x == 0 = onesToWord y
 | x == 1 = tensOneToWord y
 | x == 2 = "twenty" `mappend` onesToWord y
 | x == 3 = "thirty" `mappend` onesToWord y
 | x == 4 = "forty" `mappend` onesToWord y
 | x == 5 = "fifty" `mappend` onesToWord y
 | x == 6 = "sixty" `mappend` onesToWord y
 | x == 7 = "seventy" `mappend` onesToWord y
 | x == 8 = "eighty" `mappend` onesToWord y
 | x == 9 = "ninety" `mappend` onesToWord y

tensOneToWord :: Integer -> String
tensOneToWord x
 | x == 0 = "ten"
 | x == 1 = "eleven"
 | x == 2 = "twelve"
 | x == 3 = "thirteen"
 | x == 4 = "fourteen"
 | x == 5 = "fifteen"
 | x == 6 = "sixteen"
 | x == 7 = "seventeen"
 | x == 8 = "eighteen"
 | x == 9 = "nineteen"

onesToWord:: Integer -> String
onesToWord x
 | x == 0 = mempty
 | x == 1 = "one"
 | x == 2 = "two"
 | x == 3 = "three"
 | x == 4 = "four"
 | x == 5 = "five"
 | x == 6 = "six"
 | x == 7 = "seven"
 | x == 8 = "eight"
 | x == 9 = "nine"


