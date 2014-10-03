-- From Brent Yorgey's UPenn class (http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)

{-# OPTIONS_GHC -Wall #-}

-- rewrite the following as "Wholemeal" programs
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) 
 | even x     = (x - 2) * fun1 xs
 | otherwise  = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (product . map (\x -> x-2) . filter (even))

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = (sum . f)

f :: Integer -> [Integer]
f x 
 | x == 1    = []
 | even x    = x : f (x `div` 2)
 | otherwise = f (3*x + 1) 

-- Code Review Help to use `iterate` 
-- http://codereview.stackexchange.com/questions/63734/hailstones-and-the-colla
--foo' :: Integer -> Integer
--foo' = sum . filter even . takeWhile (/= 1) . hailstones

foo' :: Integer -> Integer
foo' = sum . filter even . takeWhile (/= 1) . hailstones

hailstones' :: Integer -> [Integer]
hailstones' x 
 | even x     = x : hailstones' (x `div` 2)
 | otherwise  = x : hailstones' (3 * x + 1)

--hailstones'' :: Integer -> [Integer]
--hailstones'' x = x : collatzStep x

collatzStep :: Integer -> Integer
collatzStep n 
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

hailstones :: Integer -> [Integer]
hailstones = iterate collatzStep

finalAnswer :: Integer -> Integer
finalAnswer = sum . filter even . takeWhile (/= 1) . hailstones


-- Given a list of Booleans, return true so long as there's an odd number (1,3,5,...)
-- True elements
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x == True then not acc else acc) False

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram  n = (map (\x -> 2*x + 1). filter (excluded)) [1..n]
     where excluded = (\x -> not $ x `elem` bad n)
     	   bad y    = numsToRemove y
    
-- I was incorrectly putting i and j in the list. The following comment helped me out:
-- http://stackoverflow.com/questions/26050460/excluding-numbers-for-sieve-of-sundaram#comment40844043_26050504
numsToRemove :: Integer -> [Integer]
numsToRemove n = [ i + j + 2*i*j | i <- [1..n], j <- [1..n], i <= j, i + j + 2*i*j <= n]

-- fold left in terms of fold right
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a) 
              deriving (Show)-- where Integer is the height (bottom = 0)

-- make balanced, binary tree (height diff is <= 1)
foldTree :: [a] -> Tree a
foldTree xs = (foldingFn . zip [0..]) xs
   where foldingFn = foldr (\(i, elem) acc -> insertElem i elem acc) Leaf
         treeHeight = getBinTreeHt xs 
         insertElem idx e tree 
           | idx `mod` 4 == 0 = keepGoingLeft  treeHeight e tree
           | idx `mod` 4 == 1 = keepGoingRight treeHeight e tree 
           | idx `mod` 4 == 2 = leftThenRight  treeHeight e tree  
           | otherwise        = rightThenLeft  treeHeight e tree

getBinTreeHt :: [a] -> Integer
getBinTreeHt = floor . (logBase 2) . fromIntegral . length  	


keepGoingLeft :: Integer -> a -> Tree a -> Tree a
keepGoingLeft index x Leaf                    = Node index Leaf x Leaf
keepGoingLeft _ x (Node level left val right) = Node level (keepGoingLeft (level-1) x left) val right

keepGoingRight :: Integer -> a -> Tree a -> Tree a
keepGoingRight index x Leaf                    = Node index Leaf x Leaf
keepGoingRight _ x (Node level left val right) = Node level left val (keepGoingRight (level-1) x right)

leftThenRight :: Integer -> a -> Tree a -> Tree a
leftThenRight index x Leaf                    = Node index Leaf x Leaf
leftThenRight _ x (Node level left val right) = Node level (keepGoingRight (level-1) x left) val right

rightThenLeft :: Integer -> a -> Tree a -> Tree a
rightThenLeft index x Leaf                    = Node index Leaf x Leaf
rightThenLeft _ x (Node level left val right) = Node level left val (keepGoingLeft (level-1) x right)

-- credit for next 2 functions: http://stackoverflow.com/a/19083798/409976
indent :: [String] -> [String]
indent = map ("  "++)

layoutTree :: Show a => Tree a -> [String]
layoutTree Leaf = []  -- wow, that was easy
layoutTree (Node _ left here right) 
         = indent (layoutTree right) ++ [show here] ++ indent (layoutTree left)
          
-- credit next 2 functions: http://codereview.stackexchange.com/questions/64047/create-binary-balanced-tree#comment117311_6404        
prettyTree :: Show a => Tree a -> String
prettyTree = unlines . layoutTree
 
main :: IO()
main = (putStrLn . prettyTree . foldTree) [0..15]