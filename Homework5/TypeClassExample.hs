{-# LANGUAGE FlexibleInstances #-}

-- Sample code for understanding type-classes of higher-kinded type
class Foo a where
  bar :: String -> a

instance Foo [String] where
	bar []    = []
	bar (x:_) = [x]