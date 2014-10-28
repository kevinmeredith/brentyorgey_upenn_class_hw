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

--instance Monoid (a -> b) where
--	mempe