{-# OPTIONS_GHC -Wall #-}
data Foo = Foo Integer

f :: Foo -> Integer
f (Foo x) = x