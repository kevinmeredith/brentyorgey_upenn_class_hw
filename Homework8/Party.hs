-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (empFun e + fun)

instance (Monoid GuestList) where
	mempty                              = GL [] 0
	mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun first@(GL _ fun1) second@(GL _ fun2) 
  | fun1 > fun2 = first
  | otherwise   = second	

data Tree a = Node {
	rootLabel :: a,
	subForest :: [Tree a]
}  deriving (Show)

treeFold :: (b -> [b] -> b) -> (a -> b) -> Tree a -> b
treeFold f g tree = f (g (rootLabel tree)) (map (g . rootLabel) (subForest tree))

add :: (Num a) => Tree a -> a
add = treeFold (\x y -> x + sum y) (id)