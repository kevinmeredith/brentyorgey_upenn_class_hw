{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips []            = []
skips [x]           = [[x]]
skips xxs@(_:_:[])  = [xxs]  
skips xxs@(_:_:xs)  = xxs : everyOther : (rest xs)
    where everyOther  = (map (\(_, y) -> y) . filter (\(x, _) -> odd x) . zip [0..]) xxs
          rest []     = []
          rest (y:ys) = [y] : rest ys 

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if (y > x && y > z) then y : localMaxima (y:z:zs) else localMaxima (y:z:zs)
localMaxima _          = []