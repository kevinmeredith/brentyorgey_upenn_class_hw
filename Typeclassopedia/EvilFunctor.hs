{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module EvilFunctor where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

data Cons a = Cons a (Cons a) | Empty deriving (Show, Eq)

instance Functor Cons where
	fmap _ Empty       = Empty
	fmap f (Cons x xs) = Cons (f x) (Cons (f x) (fmap f xs) )

instance Arbitrary (Int -> Int) where
	arbitrary = elements [(+ 5), (* 1), (* 0)]

instance Arbitrary (Cons Int) where
	arbitrary = elements [Empty, Cons 10 (Cons 100 (Cons 55 Empty)), Cons 1 Empty]

prop_id_functor_law :: Eq a => Cons a -> Bool
prop_id_functor_law x = fmap id x == id x

second_functor_law :: Eq c => (b -> c) -> (a -> b) -> Cons a -> Bool
second_functor_law f g x = left == right
  where left = fmap (f . g) $ x
        right = (fmap f) . (fmap g) $ x

makeArbitraryCons :: Gen (Cons Int)
makeArbitraryCons = arbitrary 

prop_id_functor_law_int ::  Cons Int -> Bool
prop_id_functor_law_int x = fmap id x == id x

prop_second_functor_law_int :: (Int -> Int) -> (Int -> Int) -> Cons Int -> Bool
prop_second_functor_law_int f g x = left == right
  where left = fmap (f . g) $ x
        right = (fmap f) . (fmap g) $ x

-- per this answer (http://stackoverflow.com/questions/27094789/quickcheck-tests-for-custom-type), 
-- QuickCheck *somehow* offers arbitrary Int -> Int, but needs to be Showable. I admit that I don't
-- understand the answer. But, the poster did give a work-around - give Int -> Int instances:
-- ghci> quickCheck $ prop_second_functor_law_int (+1) (*1)

-- #2 
-- it violates both laws:
-- Functor law #1 : fmap id = id
-- *Main> fmap (id) x
-- Cons 5 (Cons 5 Empty)

-- Functor law #2 fmap (f . g) = (fmap f) . (fmap g)
-- *Main> fmap (* 10) . fmap (/ 5) $ x
-- Cons 10.0 (Cons 10.0 (Cons 10.0 (Cons 10.0 Empty)))

-- *Main> fmap ((*10) . (/5)) $ x
-- Cons 10.0 (Cons 10.0 Empty)

-- *Main> x
-- Cons 5 Empty

-- #1. Give example of Functor that violates the first law, but not the second
-- law #1: fmap id = id
-- law #2: fmap (f.g) = (fmap f) . (fmap g)

data MyMaybe a = MyJust a | MyNothing deriving (Show)

instance Functor MyMaybe where
	fmap _ _ = MyNothing

instance Eq a => Eq (MyMaybe a) where
	(==) MyNothing MyNothing   = True
	(==) (MyJust x) (MyJust y) = x == y
	(==) _ _                   = False

instance Arbitrary (MyMaybe Int) where
	arbitrary = elements [MyNothing, MyJust 100, MyJust 5]

makeArbitraryMyMaybe :: Gen (MyMaybe Int)
makeArbitraryMyMaybe = arbitrary 

prop_id_functor_law_myMaybe :: MyMaybe Int -> Bool
prop_id_functor_law_myMaybe x = (id x) == (fmap id x)

prop_second_functor_law_myMaybe :: (Int -> Int) -> (Int -> Int) -> MyMaybe Int -> Bool
prop_second_functor_law_myMaybe f g x = left == right
  where left = fmap (f . g) $ x
        right = (fmap f) . (fmap g) $ x