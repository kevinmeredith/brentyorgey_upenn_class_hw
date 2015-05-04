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


import Data.Monoid
import Data.List

findFiftyOneBillion :: Char
findFiftyOneBillion = snd . head . dropWhile (\(i, _) -> i < 51000000) . zip [1..] . concat . sort . map integerToWord $ [1..999999999]

integerToWord :: Integer -> String
integerToWord x = (millions x) `mappend` (thousands x) `mappend` (hundreds x)

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
 | x == 0    = mempty `mappend` rest
 | otherwise = onesToWord x `mappend` "hundred" `mappend` rest
 where rest = tensDigitToWord (x `mod` 100) (x `mod` 10)

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

