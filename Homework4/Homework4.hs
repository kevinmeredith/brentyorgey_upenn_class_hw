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
              deriving (Show, Eq)-- where Integer is the height (bottom = 0)

-- make balanced, binary tree (height diff is 1 at most)
foldTree :: [a] -> Tree a
foldTree []     = Leaf
foldTree xxs@(x:xs) = Node level (foldTree first) x (foldTree rest)
     where (first, rest) = splitInHalf xs 
           level         = getBinTreeHt xxs

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt mid xs
    where mid = length xs `div` 2

-- Get the total height of a tree given a list of [a] to create a Tree
getBinTreeHt :: [a] -> Integer
getBinTreeHt = floor . (logBase 2) . fromIntegral . length 

-- Get the height of a tree.
getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf                  = 0
getTreeHeight (Node _ left _ right) = 1 + max (getTreeHeight left) (getTreeHeight right)

-- Checks whether tree is balanced, i.e. difference in height between the root's left and
-- right trees is <= 1
isBalanced :: Tree a -> Bool
isBalanced Leaf                  = True
isBalanced (Node _ left _ right) = diff <= 1
      where diff    = abs (leftHt - rightHt) 
            leftHt  = getTreeHeight left  
            rightHt = getTreeHeight right            

---- credit for next 2 functions: http://stackoverflow.com/a/19083798/409976
indent :: [String] -> [String]
indent = map ("  "++)

layoutTree :: Show a => Tree a -> [String]
layoutTree Leaf = []  -- wow, that was easy
layoutTree (Node _ left here right) = indent (layoutTree right) ++ [show here] ++ indent (layoutTree left)
          
---- credit next 2 functions: http://codereview.stackexchange.com/questions/64047/create-binary-balanced-tree#comment117311_6404        
prettyTree :: Show a => Tree a -> String
prettyTree = unlines . layoutTree
 
main :: IO()
main = (putStrLn . prettyTree . foldTree) [0..15]