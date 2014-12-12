{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module MonadWork where

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

---- Implement a Monad instance for ((->) e).
--instance (MyMonad (-> e)) where
--	ret x = 

-- side attempt - http://stackoverflow.com/questions/27415252/implementing-concat-for-cons-a
instance Functor Cons where
	fmap _ Empty       = Empty
	fmap f (Cons s xs) = Cons (f s) $ fmap f xs

--instance Monad Cons where
--	return x = Cons x Empty
--	m >>= f  = flatten $ fmap f m

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


