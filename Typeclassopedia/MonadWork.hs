{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module MonadWork where

import Control.Applicative
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

-- implement Monad list
data Cons a = Cons a (Cons a) 
             | Empty 
             deriving Show

class (MyMonad m) where
	ret     :: a         -> m a
	flatMap :: m a -> (a -> m b) -> m b

instance (MyMonad []) where
	ret x       = [x]
	flatMap m f = (concat . fmap (f)) m 	

-- side attempt - http://stackoverflow.com/questions/27415252/implementing-concat-for-cons-a
instance Functor Cons where
	fmap _ Empty       = Empty
	fmap f (Cons s xs) = Cons (f s) $ fmap f xs

instance Monad Cons where
    return x = Cons x Empty
    m >>= f  = flatten $ fmap f m

instance Applicative Cons where
	pure x               = Cons x Empty
	Empty <*> _          = Empty        -- why Empty? must be a law, but not sure which one.
	(Cons g gs) <*> x    = append (fmap g x) (gs <*> x)

append :: Cons a -> Cons a -> Cons a
append Empty x           = x
append x Empty           = x
append (Cons x Empty) ys = Cons x ys
append (Cons x xs) ys    = Cons x (append xs ys)


consToList :: Cons a -> [a]
consToList Empty       = []
consToList (Cons x xs) = x : consToList xs

listToCons :: [a] -> Cons a
listToCons []     = Empty
listToCons (x:xs) = Cons x $ listToCons xs

-- TODO!
-- write quickChecks tests to verify that Applicative Cons works in the same way as the real List type, `[]`
--instance Arbitrary [Int] where
--	arbitrary = elements [ [1..5], [0..100], [4,99,-444] ]

--makeArbitraryListInts :: Gen [Int]
--makeArbitraryListInts = arbitrary

-- Applicative Cons behaves "same as" Applicative [] for `pure`
pureLaw :: Int -> Bool
pureLaw x = ((consToList . consPure) x) == (listPure x)

applicativeAsteriskLaw :: [Int] -> Bool
applicativeAsteriskLaw xs = (consToList ((listToCons listFn) <*> (listToCons xs))) == (listFn <*> xs)

listPure :: Int -> [Int]
listPure = pure

consPure :: Int -> Cons Int
consPure = pure

listFn :: [Int -> Int]
listFn = [(+100)]

-- same as `concat` on the List type
flatten :: Cons (Cons a) -> Cons a
flatten Empty                = Empty
flatten (Cons (Empty) ys)    = flatten ys
flatten (Cons (Cons x xs) ys) = Cons x (flatten (Cons xs ys))

test1 :: Cons (Cons Int)
test1 = Cons (Cons 5 Empty) Empty

test2 :: Cons (Cons Int)
test2 = Cons (Cons 5 (Cons 10 Empty)) Empty

test3 :: Cons (Cons Int)
test3 = Cons (Cons 5 (Cons 10 (Cons 20 Empty))) test2

---- Implement a Monad instance for ((->) e).
-- helpful post - http://stackoverflow.com/a/27415709/409976
--instance (MyMonad (-> e)) where
--	ret x       = const x
--	flatMap m f = undefined

