{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module TrivialMonad where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

data W a = W a deriving (Show, Eq)

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

-- Exercise #1
-- no unwrapping except via `bind`
g :: Int -> W Int -> W Int
g x = bind (\y-> W (x+y)) 

-- Exercise #2
-- same as `g`s rule
h :: W Int -> W Int -> W Int
h x y = bind (\a -> (bind (\b -> W (a+b))) y) x 

instance Functor W where
	fmap f (W x) = W (f x)

instance Applicative W where
	pure x      = W x
	W f <*> W y = W (f y)

instance Monad W where
	return x  = W x
	W x >>= f = f x	

-- Exercise #3
-- Prove Monad laws hold for the Trivial Monad (W)

--1. Left identity:	return a >>= f ≡ f a

instance Arbitrary (W Int) where
	arbitrary = elements $ map (pure) [1..100] 

makeArbitraryW :: Gen (W Int)
makeArbitraryW = arbitrary

foo :: Int -> W Int
foo x = (W x)

leftIdentityLaw :: W Int -> Bool
leftIdentityLaw w@(W x) = (w >>= foo) == (foo x)

-- m >>= return ≡ m

rightIdentityLaw :: W Int -> Bool
rightIdentityLaw m = (m >>= return) == m

bar :: Int -> W Int
bar x = W (x*2) 

-- (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
associativityLaw :: W Int -> Bool
associativityLaw w = ((w >>= foo) >>= bar) == (w >>= (\x -> foo x >>= bar))
