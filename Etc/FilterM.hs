filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ []     = return []
filterM' p (x:xs) =
	let rest = filterM' p xs in 
	do b <- p x
	   if (b) then rest >>= (\a -> return (x : a)) else rest

-- filterM' (const $ [True, False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
