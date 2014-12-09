{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module TrivialMonad where

data W a = W a deriving Show

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

-- no unwrapping except via `bind`
g :: Int -> W Int -> W Int
g x = bind (\y-> W (x+y)) 
