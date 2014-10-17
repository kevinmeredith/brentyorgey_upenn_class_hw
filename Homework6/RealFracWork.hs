f :: Integer -> Integer
f x 
 | odd x     = 0
 | otherwise = (floor . logBase (fromIntegral 2)) (fromIntegral (largestPowerDivBy2 x))

largestPowerDivBy2 :: Integer -> Integer
largestPowerDivBy2 x = largestPowerDivBy2' x 0
                  where largestPowerDivBy2' num acc = if num `rem` (2^(acc+1)) == 0 then largestPowerDivBy2' num (acc+1)
           	                                          else (2^acc)

--[1,2,3,4,5,6,7]

--[0,1,0,2,0,1,0, ]

-- 16 == x 

-- 2 ^ 4 == y

-- x `rem` y == 0?