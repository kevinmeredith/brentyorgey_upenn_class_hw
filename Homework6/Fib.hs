import Data.Map as M 

fib :: Integer -> Integer
fib n
 | n == 0    = 0
 | n == 1    = 1
 | otherwise = fib (n-1) + fib (n-2)

-- infinite list of fibs
fibs :: [Integer]
fibs = Prelude.map fib [0..]

-- improved fibs using memoized (of already calculated fibonacci values)
fibsImproved :: [Integer]
fibsImproved = fibsImproved' 0 M.empty

fibsImproved' :: Integer -> Map Integer Integer -> [Integer]
fibsImproved' n memoized  = case maybeMember of Just x  -> x : fibsImproved' (n+1) memoized
                                                Nothing -> calced : fibsImproved' (n+1) (M.insert n calced $ memoized)
                            where maybeMember = M.lookup n memoized
                                  calced      = fib n