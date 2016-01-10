import Data.Traversable as T
import Control.Applicative

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance (Functor Tree) where
	fmap _ Empty        = Empty
	fmap f (Leaf x)     = Leaf (f x)
	fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

--traverse
--  :: (Traversable t, Control.Applicative.Applicative f) =>
--     (a -> f b) -> t a -> f (t b)

instance (Foldable Tree) where
  foldr f acc Empty        = acc
  foldr f acc (Leaf x)     = f x acc
  foldr f acc (Node l x r) = foldr f (foldr f (f x acc) r) l

  -- (a -> b -> b) -> b -> t a -> b 

--class (Functor t, Foldable t) => Traversable (t :: * -> *)
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance (Traversable Tree) where
	traverse _ Empty        = pure Empty
	traverse f (Leaf x)     = Leaf <$> (f x)
	traverse f (Node l x r) = Node <$> (traverse f l) <*> f x <*> (traverse f r)