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

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

-- (a -> b) -> (ITree a) -> (ITree b)    

instance Functor (ITree) where
    fmap f (Leaf g)  = Leaf $ f . g
    fmap f (Node xs) = Node $ go xs
                       where go (y:ys) = f y : go ys
                             go []     = []
