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

-- Show 20 items of the list rather than infinitity
instance Show a => Show (Stream a) where
	show stream = show $ go stream 1 
	                     where go (Stream (x, rest)) count 
	                            | count == 20 = []
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
-- it has the 'Cheat' suffix since it's dividing
rulerCheat :: Stream Integer
rulerCheat = streamMap doDivBy2 startAtOne
    where startAtOne = streamFromSeed 1 (+1) :: Stream Integer

-- look at RealFracWork.hs to solve type problem
-- note that `doDivBy2` is a partial, hack function
doDivBy2 :: Integer -> Integer
doDivBy2 x = doDivBy2' x 0
              where doDivBy2' 1 acc = acc
                    doDivBy2' y acc = doDivBy2' (y `div` 2) (acc+1)

--interleave :: Stream a -> Stream a -> Stream a
--interleave 

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...

-- every other 0

-- , 1 2 1 3 1 2 1 4, ...

-- every other is 1
-- 2, 3, 2, 4

-- 010x where x = 2, 3, 2, 4

-- hypothesis: 010x where odd instance is 2, otherwise increasing number (3, 4, ...)
-- need to prove by completing above `rulerCheat`
