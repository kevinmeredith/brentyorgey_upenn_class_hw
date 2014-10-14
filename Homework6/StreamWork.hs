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

--streamMap :: (a -> b) -> Stream a -> Stream b
--streamMap f (Stream x) = Stream $ f x 

--streamFromSeed :: a -> (a -> a) -> Stream a
