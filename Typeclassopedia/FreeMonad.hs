{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module MonadWork where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

-- Exercise 3: Implement Functor and Monad instances for Free f.
-- Assume that `f` has a Functor instance
data Free f a = Var a
               | Node (f (Free f a)) 

instance Functor f => Functor (Free f) where
  fmap g (Var x)  = Var (g x)
  fmap g (Node x) = Node $ fmap (\y -> fmap g y) x

instance (Eq a) => Eq (Free Maybe a) where
	(==) (Var x) (Var y)       = x == y
	(==) (Node fu1) (Node fu2) = fu1 == fu2
	(==) _ _ 			       = False

instance (Show a) => Show (Free Maybe a) where
  show (Var x)          = "Var " ++ (show x)
  show (Node (Just xs)) = "Node $ Just $ " ++ (show xs)
  show (Node Nothing)   = "Node Nothing"

-- Functor Laws (from Typeclassopedia):
instance Arbitrary (Free Maybe Int) where
	arbitrary = do
		x <- arbitrary :: Gen Int
		y <- arbitrary :: Gen Int
		elements [Var x, Var y, Node (Nothing), Node (Just (Var y))] 

--fmap id = id
--fmap (g . h) = (fmap g) . (fmap h)

functor_id_law ::  Free Maybe Int -> Bool
functor_id_law x = (fmap id x) == (id x)

--functor_compose_law :: (Int -> Int) -> (Int -> Int) -> Cons Int -> Bool
--functor_compose_law f g x = left == right
--  where left = fmap (f . g) $ x
--        right = (fmap f) . (fmap g) $ x

