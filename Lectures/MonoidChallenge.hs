{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}

import Data.Monoid

-- Challenge: Make Bool an instance of Monoid
data Bool' = T | F deriving (Show)

instance Monoid (Bool') where
	mempty = T
	mappend T _ = T
	mappend _ T = T
	mappend _ _ = F 


-- Challenge: Make function an instance of Monoid

class Monoid' a where
	mempty  :: a -> a
	mappend :: a -> a -> a

instance Monoid' (a -> b) where
    mempty f    = f
    mappend f g = f . g

g :: Integer -> Integer
g = (+ 10)

-- :t g . g == Integer -> Integer