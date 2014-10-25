{-# OPTIONS_GHC -Wall #-}
module StreamRefined where

-- Stream is an element followed by a Stream
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x str) = x : streamToList str

-- Show 20 items of the list rather than infinitity
instance Show a => Show (Stream a) where
	show stream = show $ go stream 1 
	                     where go (Cons x rest) count 
	                            | count == 20 = []
	                            | otherwise   = x : go rest (count+1)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)	                            

--interleave :: Stream a -> Stream a -> Stream a
--interleave (Cons x strX) (Cons y strY) = Cons x (Cons y (interleave strX strY))

interleave :: Stream a -> Stream a -> Stream a
interleave (Cons x xs) ys = Cons x (interleave ys xs)

ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = interleave (streamRepeat n) (ruler' (n+1))