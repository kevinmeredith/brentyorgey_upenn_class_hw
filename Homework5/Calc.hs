{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x)     = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2	

evalStr :: String -> Maybe Integer
evalStr xs = parseExp Lit Add Mul xs >>= (\x -> Just $ eval x)

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a

instance Expr ExprT where
	lit x     = Lit x
	add e1 e2 = Add e1 e2
	mul e1 e2 = Mul e1 e2