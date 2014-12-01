{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module ApplicativeWork where

import Control.Applicative

-- Prove that
--    pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- Prelude> :t flip ($)
-- flip ($) :: b -> (b -> c) -> c

-- pure (flip ($)) <*> x
-- (b -> c) -> c

-- pure (flip ($)) <*> x <*> pure f
-- c 

-- pure f <*> x 
-- c

-- equal

-- Exercise #1: Make the Applicative Instance for Maybe
data Option a = Some a 
                | None deriving Show

instance Functor Option where
   fmap _ None     = None
   fmap f (Some x) = Some (f x)

instance Applicative Option where
   pure x                = Some x
   (Some f) <*> (Some x) = Some (f x) 
   _ <*> _               = None  

-- Exercise #2: Determine the correct definition of pure for the ZipList instance of Applicative—there 
-- is only one implementation that satisfies the law relating pure and (<*>).   
newtype ZList a = ZList { getZList :: [a] } deriving Show
 
instance Functor ZList where
	fmap f (ZList xs) = ZList (fmap f xs)

instance Applicative ZList where
  pure x                    = ZList (repeat x)
  (ZList gs) <*> (ZList xs) = ZList (zipWith ($) gs xs)

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

-- Exercise: Implement pure and (<*>) in terms of unit and (**), and vice versa.

--class Functor f => MyApplicative f where
--	pure 