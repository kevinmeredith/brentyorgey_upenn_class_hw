import Data.Map as M 

fib :: Integer -> Integer
fib n
 | n == 0    = 0
 | n == 1    = 1
 | otherwise = fib (n-1) + fib (n-2)

-- infinite list of fibs
fibs :: [Integer]
fibs = Prelude.map fib [0..]

-- improved fibs using accumulator (of already calculated fibonacci values)
fibsImproved :: [Integer]
fibsImproved = fibsImproved' 0 M.empty 0
        where fibsImproved' n memoized acc 
               | n == 0         = acc
               | M.member n acc = fibsImproved' (n-1) memoized ((M.! memoized n) + acc)    -- M.member feels wrong
               | otherwise      = let calced = fib n 
                                  in fibsImproved' (n-1) (M.insert n calced $ acc) (calced + acc)