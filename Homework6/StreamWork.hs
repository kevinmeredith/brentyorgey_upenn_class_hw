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
ruler :: Stream Integer
ruler = streamMap f startAtOne
    where f x        = if (odd x) then 0 else g x 0
          g y acc    = if y == 0 then acc else g (y-2) (acc+1)
          startAtOne = streamFromSeed 1 (+1)