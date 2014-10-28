data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Show)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

maxTreeElem :: (Ord a) => Tree a -> Maybe a
maxTreeElem Empty             = Nothing
maxTreeElem tree@(Node l x r) = Just $ treeFold x (\x y z -> max x $ max y z) tree

sumTreeElem :: (Num a) => Tree a -> a
sumTreeElem = treeFold 0 (\x y z -> x + y + z)

-- *Main> treeFold 0 (\x y z -> max x $ max y z) t
-- 500

