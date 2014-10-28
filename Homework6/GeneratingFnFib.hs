-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module GeneratingFnFib where

import StreamRefined

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
	fromInteger x = Cons x $ streamRepeat 0
	negate        = streamMap (* (-1)) 
	(+) xs ys     = combineStreams (+) xs ys
	(*) xs ys     = multStreams xs ys
	abs           = streamMap abs

--Multiplication is a bit trickier. Suppose A = a0 + xA` and B = b0 +
-- xB0 are two generating functions we wish to multiply. We reason as
-- follows: AB = (a0 + xA`)B   
--             = a0B + xA`B    
--             = a0(b0 + xB0) + xA`B   
--             = a0b0 + x(a0B0 + A`B)

-- got help from http://stackoverflow.com/a/26579938/409976
-- TODO: http://stackoverflow.com/questions/26579781/multiplying-streams-representing-polynomial-coefficients#comment41777117_26579938
multStreams :: Stream Integer -> Stream Integer -> Stream Integer
multStreams (Cons x xs) b@(Cons y ys) = 
	Cons (x * y) (streamMap (*x) ys + multStreams xs b)

-- division of streams
-- Q = (a0/b0) + x((1/b0)(A` − QB`)).

instance Fractional (Stream Integer) where 
	(/) xs ys = divStreams xs ys

divStreams :: Stream Integer -> Stream Integer -> Stream Integer
divStreams (Cons x xs) (Cons y ys) =
	Cons (x `div` y) ()
