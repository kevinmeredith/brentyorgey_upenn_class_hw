-- writing out functions (to understand them) from
-- this paper: http://staff.city.ac.uk/~ross/papers/Applicative.html

import Control.Applicative

transpose :: [[a]] -> [[a]]
transpose []        = repeat []
transpose (xs: xss) = zipWith (:) xs (transpose xss)

-- waiting on answer from StackOverflow

dist :: Applicative f => [f a] -> f [a]
dist []     = pure []
dist (v:vs) = pure (:) <*> v <*> (dist vs)

flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f ss = dist (fmap f ss)

traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse f []     = pure []
traverse f (x:xs) = pure (:) <*> (f x)  <*> (traverse f xs)

--transpose' :: [[a]] -> [[a]]
--transpose' []       = [[]]
--transpose' (xs:xss) = [(:) xs (transpose' xss)]