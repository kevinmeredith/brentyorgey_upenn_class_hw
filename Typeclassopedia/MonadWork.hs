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

-- Implement a Monad instance for ((->) e).
instance (MyMonad (-> e)) where
	ret x = 

-- side attempt - http://stackoverflow.com/questions/27415252/implementing-concat-for-cons-a
instance Functor Cons where
	fmap _ Empty       = Empty
	fmap f (Cons s xs) = Cons (f s) $ fmap f xs

--instance Monad Cons where
--	return x = Cons x Empty
--	m >>= f  = flatten $ fmap f m

--flatten :: Cons (Cons a) -> Cons a
--flatten Empty               = Empty
--flatten (Cons c@(Cons _ _)) = c






