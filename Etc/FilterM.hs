filterM' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
filterM' _ []       = pure []
filterM' f (x : xs) = let first = pure (\bool -> if (bool) then [x] else []) <*> (f x)
                      in (fmap (++) first) <*> filterM' f xs