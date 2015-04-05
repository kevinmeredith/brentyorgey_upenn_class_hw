{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module MonadWork where

import Control.Applicative
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

-- implement Monad list
data Cons a = Cons a (Cons a) 
             | Empty 
             deriving Show

class (MyMonad m) where
	ret     :: a         -> m a
	flatMap :: m a -> (a -> m b) -> m b

class (MyFunctor f) where
    fumap :: (a -> b) -> f a -> f b	

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
append (Cons x xs) ys    = Cons x (append xs ys)

consToList :: Cons a -> [a]
consToList Empty       = []
consToList (Cons x xs) = x : consToList xs

listToCons :: [a] -> Cons a
listToCons []     = Empty
listToCons (x:xs) = Cons x $ listToCons xs

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
-- waiting for help on whether to pass in a type after ->
--type Arr a b = a -> 

instance MyFunctor ((->) e) where
	fumap = (.)

instance (MyMonad ((->) e)) where
	ret         = const
	flatMap m f = \r -> f (m r) r

-- 	flatMap :: m a -> (a -> m b) -> m b

-- Exercise 3: Implement Functor and Monad instances for Free f.
-- Assume that `f` has a Functor instance
data Free f a = Var a
               | Node (f (Free f a)) 

instance Functor f => Functor (Free f) where
  fmap g (Var x)  = Var (g x)
  fmap g (Node x) = Node $ fmap (\y -> fmap g y) x

-- TODO: finish `free monad` instances
-- see http://stackoverflow.com/questions/27527703/implementing-applicative-free-f#comment43484816_27527703 for help

instance Functor f => Applicative (Free f) where
    pure                = Var
    (Var f) <*> x       = fmap f x  -- from answer: pure f <*> x is fmap f x
    u       <*> (Var y) = pure ($ y) <*> u -- interchange law: u <*> pure y = pure ($ y) <*> u

--instance Applicative f => Monad (Free f) where
--	return  x      = Var x
--	(Var x) >>= f  = f x
--	(Node x) >>= f = Node $ fmap (\y -> y >>= f) x

  -- a -> b 
  -- a -> Free f b

-- (>>=) in terms of fmap, pure, and (<*>). We are given a value x 
-- of type m a, and a function k of type a -> m b               
-- :t (>>=) :: m a -> (a -> m b) -> m b
-- :t (<*>) :: f (a -> b) -> f a -> f b
-- k >>= x = pure (x) <*> k 
--         = m (a -> m b) <*> m a 

-- Ex 1: Implement (>>=) in terms of fmap (or liftM) and join.
bind :: (Functor m, Monad m) => m a -> (a -> m b) -> m b
bind x f = join $ fmap f x

-- Ex 2: Now implement join and fmap (liftM) in terms of (>>=) and return.
join' :: Monad m => m (m a) -> m a
join' x = x >>= id 

fmap' :: (Functor f, Monad f) => (a -> b) -> f a -> f b
fmap' f x = x >>= return . f

-- Node (Just "foo") :: Node Maybe Int