{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module MonadWork where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Text.Show.Functions
import Control.Applicative

-- Exercise 3: Implement Functor and Monad instances for Free f.
-- Assume that `f` has a Functor instance
data Free f a = Var a
               | Node (f (Free f a)) 

instance Functor f => Functor (Free f) where
  fmap g (Var x)  = Var (g x)
  fmap g (Node x) = Node $ fmap (\y -> fmap g y) x

instance Functor f => Applicative (Free f) where
	pure           = Var
	(Var f) <*> x  = fmap f x
	(Node f) <*> x = Node $ fmap (\g -> g <*> x) f

instance (Eq (f (Free f a)), Eq a) => Eq (Free f a) where
	(==) (Var x) (Var y)       = x == y
	(==) (Node fu1) (Node fu2) = fu1 == fu2
	(==) _ _ 			       = False

instance (Show (f (Free f a)), Show a) => Show (Free f a) where
  show (Var x)  = "Var " ++ (show x)
  show (Node x) = "Node " ++ (show x)

instance Arbitrary (Free Maybe Int) where
	arbitrary = do
		x <- arbitrary :: Gen Int
		y <- arbitrary :: Gen Int
		elements [Var x, Var y, Node (Nothing), Node (Just (Var y))] 

instance Arbitrary (Free Maybe (Int -> Int)) where
	arbitrary = do
		f <- arbitrary :: Gen (Int -> Int)
		elements [Var f, Var f, Node (Nothing), Node (Just (Var f))] 

-- Functor Laws (from Typeclassopedia):
-- (1) fmap id = id
-- (2) fmap (g . h) = (fmap g) . (fmap h)

functor_id_law ::  Free Maybe Int -> Bool
functor_id_law x = (fmap id x) == (id x)

functor_compose_law :: (Int -> Int) -> (Int -> Int) -> Free Maybe Int -> Bool
functor_compose_law f g x = left == right
  where left = fmap (f . g) $ x
        right = (fmap f) . (fmap g) $ x

-- Applicative Laws 
-- (1) pure id <*> v = v
-- (2) pure f <*> pure x = pure (f x)
-- (3) u <*> pure y = pure ($ y) <*> u
-- (4) u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

applicative_id_law :: Free Maybe Int -> Bool
applicative_id_law x = (pure id <*> x) == x

applicative_homomorphism_law :: (Int -> Int) -> Int -> Bool
applicative_homomorphism_law f x = left == right where
	left  = (pure f <*> pure x) :: Free Maybe Int
	right = pure (f x)          :: Free Maybe Int

applicative_interchange_law :: Free Maybe (Int -> Int) -> Int -> Bool
applicative_interchange_law f x = left == right where
	left  = f <*> pure x     
	right = pure ($ x) <*> f 

applicative_composition_law :: Free Maybe (Int -> Int) -> Free Maybe Int -> Bool
applicative_composition_law f x = left == right where
	left  = f <*> (f <*> x) 
	right = pure (.) <*> f <*> f <*> x
