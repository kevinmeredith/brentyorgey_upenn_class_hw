-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty               y                    = y
(+++) x                   Empty                = x
(+++) left@(Single _ _)   right@(Single _ _)   = Append (tag $ Append mempty left right) left right
(+++) left                right                = Append (tag left `mappend` tag right) left right


tag :: Monoid m => JoinList m a -> m
tag Empty                 = mempty
tag (Single x _)          = x
tag (Append x left right) = x  --`mappend` (tag left) `mappend` (tag right)

jl1 :: JoinList (Product Integer) String
jl1 = Append (Product 100) (Single (Product 25) "foo") (Single (Product 4) "bar")

jl2 :: JoinList (Product Integer) String
jl2 = Append (Product 50) (Single (Product 25) "bippy") (Single (Product 2) "baz")