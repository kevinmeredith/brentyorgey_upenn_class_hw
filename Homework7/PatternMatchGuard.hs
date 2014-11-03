foo :: Integer -> Maybe Integer
foo x | x == 10 = Just 10
foo _           = Nothing