{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module ApplicativeWork where

import Control.Applicative

-- Prove that
--    pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- ghci> :t pure f <*> x
-- pure f <*> x :: (Applicative f, Num b) => f b
-- ghci> :t pure (flip ($)) <*> pure 10
-- pure (flip ($)) <*> pure 10
--   :: (Applicative f, Num a) => f ((a -> c) -> c)
-- ghci> :t pure (flip ($))
-- pure (flip ($)) :: Applicative f => f (b -> (b -> c) -> c)
-- ghci> :t pure (flip ($)) <*> pure 10
-- pure (flip ($)) <*> pure 10
--   :: (Applicative f, Num a) => f ((a -> c) -> c)
-- ghci> :t pure (flip ($)) <*> pure 10 <*> pure f
-- pure (flip ($)) <*> pure 10 <*> pure f
--  :: (Applicative f, Num b) => f b
-- ghci> pure (flip ($)) <*> pure 10 <*> pure f
-- 11

-- Exercise #1: Make the Applicative Instance for Maybe
data Option a = Some a 
                | None deriving Show

instance Functor Option where
   fmap _ None     = None
   fmap f (Some x) = Some (f x)

instance Applicative Option where
   pure x                = Some x
   (Some f) <*> (Some x) = Some (f x) -- should I use `fmap` here?
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
  u :: f ()
  dotdot :: f a -> f b -> f (a,b)

-- Exercise: Implement pure and (<*>) in terms of unit and (**), and vice versa.

-- modifying names of functions in order to prevent clash with Prelude
class Functor f => MyApplicative f where
  p     :: a -> f a                  
  apply :: f (a -> b) -> f a -> f b

-- my initial implementation
instance Monoidal Option where
  u                        = p ()
  dotdot None _            = None 
  dotdot _ None            = None
  dotdot (Some x) (Some y) = apply (Some id) (Some (x, y))

instance MyApplicative Option where
  p                = Some
  apply None _     = None
  apply _ None     = None
  apply (Some g) f = fmap g f 

-- help from StackOverflow (http://stackoverflow.com/a/23320391/409976)

-- pure a = fmap (const a) unit
-- unit = pure ()

-- ff <*> fa = fmap (\(f, a)) $ ff ** fa
-- fa ** fb = pure (,) <*> ff <*> fb