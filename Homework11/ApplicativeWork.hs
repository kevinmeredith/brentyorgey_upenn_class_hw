{-# OPTIONS_GHC -Wall #-}

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
(*>) _ f = f

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA _ []     = pure []
mapA f (x:xs) = (:) <$> f x <*> mapA f xs

-- see codereview for improvement
-- http://codereview.stackexchange.com/questions/75397/implementing-sequencea
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (++) <$> (fmap (\y -> [y]) x) <*> sequenceA xs
