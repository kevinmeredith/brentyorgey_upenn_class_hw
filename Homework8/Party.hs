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

-- original, not standard 
-- thanks to @ski on #haskell
treeFold :: (b -> [b] -> b) -> (a -> b) -> Tree a -> b
treeFold f g tree = f (g (rootLabel tree)) (map (treeFold f g) (subForest tree)) 

treeFold' :: (a -> [b] -> b) -> Tree a -> b
treeFold' f (Node x ts) = f x (map (treeFold' f) ts)

add :: (Num a) => Tree a -> a
add = treeFold (\x y -> x + sum y) (id)

concat' :: Tree String -> [String]
concat' = treeFold (\x y -> x ++ (foldr (++) [] y)) (\x -> [x])

stringTree :: Tree String
stringTree = Node { rootLabel = "foo", subForest = [t1, t2, t3] }

t1 :: Tree String
t1 = Node { rootLabel = "bar", subForest = [] }

t2 :: Tree String
t2 = Node { rootLabel = "bippy", subForest = [] }

t3 :: Tree String
t3 = Node { rootLabel = "baz", subForest = [] }

toList :: Tree a -> [a]
toList = treeFold (\x y -> x ++ (foldr (++) [] y)) (\x -> [x])

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel _ xs = (with, without)
  where with    = foldr (\(elem@(GL _ fun), _) acc@(GL _ accFun) -> if fun > accFun then elem else acc) zero xs
        without = foldr (\(_, elem@(GL _ fun)) acc@(GL _ accFun) -> if fun > accFun then elem else acc) zero xs
        zero    = (GL [] 0)

e1 :: Employee
e1 = Emp { empName = "foo", empFun = 100 }

e2 :: Employee
e2 = Emp { empName = "bar", empFun = 300 }

e3 :: Employee
e3 = Emp { empName = "baz", empFun = 300 }

e4 :: Employee
e4 = Emp { empName = "bippy", empFun = 400 }

guestListWith :: GuestList
guestListWith = GL [e1, e2, e3, e4] 500

guestListWithout :: GuestList
guestListWithout = GL [e1, e2, e3, e4] 10000

--	maxFun :: Tree Employee -> GuestList
