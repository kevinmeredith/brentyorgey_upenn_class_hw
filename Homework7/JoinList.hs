-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty               y                    = y
(+++) x                   Empty                = x
(+++) left                right                = Append (tag left `mappend` tag right) left right

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x  --`mappend` (tag left) `mappend` (tag right)

jl1 :: JoinList (Product Integer) String
jl1 = Append (Product 100) (Single (Product 25) "foo") (Single (Product 4) "bar")

jl2 :: JoinList (Product Integer) String
jl2 = Append (Product 50) (Single (Product 25) "bippy") (Single (Product 2) "baz")

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                 = Nothing
indexJ i (Single _ x) 
  | i == 0    = Just x
  | otherwise = Nothing
indexJ i (Append _ left right)
  | leftHt > i  = indexJ i left
  | (rightHt + leftHt) > i = indexJ (i-leftHt) right
  | otherwise    = Nothing
  where leftHt = (getSize. size . tag) left
        rightHt = (getSize. size . tag) right


jlIndex1 :: JoinList Size String
jlIndex1 = Append (Size 2) (Single (Size 1) "foo") (Single (Size 1) "bar")

jlIndex2 :: JoinList Size String
jlIndex2 = Append (Size 3) (Single (Size 1) "foo") (Append (Size 2) (Single (Size 1) "bar") (Single (Size 1) "baz"))

jlIndex3 :: JoinList Size String
jlIndex3 = Append (Size 4) Empty

--dropJ ::(Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
--dropJ _ Empty = Empty
--dropJ i single@(Single _ _)
--  | i == 0    = single
--  | otherwise = Empty
--dropJ i append@(Append _ left right) 
--  | i == 0                           = append
--  | (getSize . size . tag) left >  i = indexJ i left
--  | otherwise                        = indexJ i right

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jToList :: JoinList m a -> [a]
jToList Empty          = []
jToList (Single _ x)   = [x]
jToList (Append _ x y) = jToList x ++ jToList y

-- testing via QuickCheck
