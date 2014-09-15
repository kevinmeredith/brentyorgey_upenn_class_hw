split :: Char -> String -> [String]
split x ys = split' ys [] 
              where split' :: String -> String -> [String]
              	    split' [] acc = [reverse acc]
              	    split' (z:zs) acc  
                     | x == z            = (reverse acc) : split' zs []
                     | otherwise         = split' zs (z:acc)                                   

split2 :: (Eq a) => a -> [a] -> [[a]]
split2 _ [] = []
split2 x ys = f : split2 x rest 
  where (f, rest) = break (== x) (dropWhile (== x) ys) -- "" at end when splitter on end