newtype Foo a = Foo { bar :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c) 

patternMatch :: Foo a -> Maybe (a, String)
patternMatch (Foo f) = f "foo"

newtype Bar a = Bar { biz :: Int -> Int -> a }

