{- initially (partially) re-invented the wheel, but then found http://stackoverflow.com/questions/9814648/haskell-list-of-data-type-sorting
dumbSort :: [LogMessage] -> [LogMessage]
dumbSort [] = []
dumbSort xs = if (isSorted xs) then xs else dumbSort $ addMinToFront xs

isSorted :: (Eq a) => [a] -> Bool
isSorted []         = True
isSorted xxs@(x:xs) = if (minimum xxs == x) then (isSorted xs) else False

addMinToFront :: [a] -> [a]
addMinToFront []   = []
addMinToFront xs = min' : filterOne min'
                   where min' = minimum xs

-- >>= :: (Monad m) => m a -> (a -> m b) -> m b

safeMinimum :: [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just $ minimum xs

filterOne :: (Eq a) =>  a ->  [a] -> [a]
filterOne _ [] = []
filterOne y xs = reverse $ filterOne' xs []
                where filterOne' (z:zs) acc = if (z == y) then (zs ++ acc) else filterOne' zs (z : acc)

--Node MessageTree LogMessage MessageTree
-}