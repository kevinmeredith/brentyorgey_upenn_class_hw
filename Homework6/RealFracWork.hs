f :: Integer -> Integer
f x = if (odd x) then 0 else (floor . logBase 2) x