{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}

import Data.Functor

-- #1
data MyEither e a = MyRight a 
                  | MyLeft e 
                  deriving Show

instance Functor (MyEither e) where
    fmap _ (MyLeft e)  = MyLeft e
    fmap f (MyRight x) = MyRight (f x)

--instance Functor ((->) e) where
--	fmap f g = (.)

data Comma a b = C (a, b) deriving Show

instance Functor (Comma a) where
	fmap f (C (x, y)) = C (x, f y)

data Pair a = P (a, a) deriving Show

instance Functor (Pair) where
    fmap f (P (x, y)) = P (f x, f y)

data ITree a = Leaf (Int -> a) | Node [ITree a]

-- (a -> b) -> (ITree a) -> (ITree b)    

instance Functor (ITree) where
	fmap = iTreeMap

iTreeMap :: (a -> b) -> ITree a -> ITree b
iTreeMap f (Leaf x)  = Leaf $ fmap f x
iTreeMap f (Node xs) = Node $ map (iTreeMap f) xs

foldTree' :: ((Int -> a) -> b) -> ([b] -> b) -> ITree a -> b
foldTree' f _ (Leaf x)  = f x
foldTree' f g (Node xs) = g $ map (foldTree' f g) xs

--foldTree' :: (a -> [b] -> b) -> ITree a -> b
--foldTree' f (Leaf x)  = f x []
--foldTree' f (Node xs) = f _ $ map (foldTree f) xs  

-- #4
-- Give an example of a type of kind * -> * which cannot be made an 
-- instance of Functor (without using undefined).

--data Foo a = Foo (a -> Bool)

--instance Functor (Foo) where
--  fmap f (Foo g) = Foo (f . g) 

-- #5
-- The composition of two Functors is also a Functor. 
-- helped: http://stackoverflow.com/questions/19774564/what-does-it-mean-to-compose-two-functors
newtype Comp f g a = Comp { unComp :: f (g a) }

compose :: f (g a) -> Comp f g a
compose = Comp

decompose :: Comp f g a -> f (g a)
decompose = unComp

instance (Functor f, Functor g) => Functor (Comp f g) where
	fmap f = compose . fmap (fmap f) . decompose 

data Cons a = Empty | Cons a (Cons a) deriving Show

---- evil functor
instance Functor Cons where
	fmap _ Empty       = Empty
	fmap f (Cons x xs) = Cons (f x) (Cons (f x) (fmap f xs))

-- extract first Cons 
foo :: Cons a -> Maybe a
foo Empty      = Nothing
foo (Cons a _) = Just a 
