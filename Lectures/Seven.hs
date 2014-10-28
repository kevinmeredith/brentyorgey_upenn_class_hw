data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Show)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

-- should be Bounded type-class instance per Prof. Yorgey's lecture
maxTreeElem :: (Ord a) => Tree a -> Maybe a
maxTreeElem Empty             = Nothing
maxTreeElem tree@(Node l x r) = Just $ treeFold x (\x y z -> max x $ max y z) tree

-- *Main> treeFold 0 (\x y z -> max x $ max y z) t
-- 500

sumTreeElem :: (Num a) => Tree a -> a
sumTreeElem = treeFold 0 (\x y z -> x + y + z)

treeHeight :: Tree a -> Integer
treeHeight = treeFold 0 (\l _ r -> 1 + max l r)

treeSize :: Tree a -> Integer
treeSize = treeFold 0 (\l _ r -> 1 + l + r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r) 

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit x)     = f x
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2) 
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)  

