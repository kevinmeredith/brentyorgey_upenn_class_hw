{-# OPTIONS_GHC -Wall #-}
module StreamRefined ( Stream(Cons), streamToList, streamRepeat, streamMap, interleave, ruler, combineStreams ) where

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

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

combineStreams :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
combineStreams f (Cons x xs) (Cons y ys) = Cons (f x y) (combineStreams f xs ys)

--interleave :: Stream a -> Stream a -> Stream a
--interleave (Cons x strX) (Cons y strY) = Cons x (Cons y (interleave strX strY))

-- mjolka (on Stack Exchange) explained to me why my original `interleave` was wrong.
-- Bottom line - we think that pattern matching on the second argument never terminated
--               since the second argument of `interleave` (when called by ruler') always was
--               infinitely recursive.
interleave :: Stream a -> Stream a -> Stream a
interleave (Cons x xs) ys = Cons x (interleave ys xs)

ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = interleave (streamRepeat n) (ruler' (n+1))