-- writing out functions (to understand them) from
-- this paper: http://staff.city.ac.uk/~ross/papers/Applicative.html

transpose :: [[a]] -> [[a]]
transpose []        = repeat []
transpose (xs: xss) = zipWith (:) xs (transpose xss)