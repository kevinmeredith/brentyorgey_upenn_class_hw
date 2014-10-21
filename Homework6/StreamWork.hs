{-# OPTIONS_GHC -Wall #-}
module StreamWork where

-- Stream is an element followed by a Stream
data Stream a = Stream (a, Stream a) 

-- convert Stream to infinite list (Note - there's no empty Stream)
streamToList :: Stream a -> [a]
streamToList (Stream x) = hd : streamToList stream
      where (hd, stream) = x

-- a bit cleaner
streamToList' :: Stream a -> [a]
streamToList' (Stream (x, rest)) = x : streamToList' rest

-- Show 30 items of the list rather than infinitity
instance Show a => Show (Stream a) where
	show stream = show $ go stream 1 
	                     where go (Stream (x, rest)) count 
	                            | count == 30 = []
	                            | otherwise   = x : go rest (count+1)

streamRepeat :: a -> Stream a
streamRepeat x = Stream (x, streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream (x, rest)) = Stream (f x, streamMap f rest)

-- Given a start value and function, generate a stream by applying fn to each Stream value
streamFromSeed :: a -> (a -> a) -> Stream a
streamFromSeed x f = Stream $ (x, streamFromSeed (f x) f)

-- stream of natural numbers (0,1,2,...)
nat :: Stream Integer
nat = streamFromSeed 0 (+1)

-- `ruler` function
--where the nth element in the stream (assuming the first element
--corresponds to n = 1) is the largest power of 2 which evenly
--divides n
-- it has the 'Cheat' suffix since it's dividing (as ruled out by the instructions)
--rulerCheat :: Stream Integer
--rulerCheat = streamMap f startAtOne
--    where startAtOne = streamFromSeed 1 (+1) :: Stream Integer

-- ruler function starts with 0 and every other is 0
-- starting with 1, it repeats (between 0s) to (2^1 - 1) time


--ruler :: Stream Integer
--ruler = interleave zeros alternating
--  where zeros       = streamRepeat 0
--        alternating = streamFromSeed 1 ()

interleave :: Stream a -> Stream a -> Stream a
interleave (Stream(x, str1)) (Stream(y, str2)) = Stream(x, Stream(y, rest))
          where rest = interleave str1 str2

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...

-- every other 0

-- , 1 2 1 3 1 2 1 4, ...

-- every other is 1
-- 2, 3, 2, 4

-- 010x where x = 2, 3, 2, 4

-- hypothesis: 010x where odd instance is 2, otherwise increasing number (3, 4, ...)
-- need to prove by completing above `rulerCheat`
