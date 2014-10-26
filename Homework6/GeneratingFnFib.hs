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
	(*) xs ys     = ys                             -- TODO: fix 
	abs           = streamMap abs
