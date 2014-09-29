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

reify :: ExprT -> ExprT
reify = id

class Expr a where
	lit :: Integer        -> a
	add :: ExprT -> ExprT -> a
	mul :: ExprT -> ExprT -> a

instance Expr Integer where
	lit x     = x
	add e1 e2 = eval e1 + eval e2 
	mul e1 e2 = eval e1 * eval e2

instance Expr Bool where
	lit x     = x > 0  
	add e1 e2 = eval e1 > 0 || eval e2 > 0 
	mul e1 e2 = eval e1 > 0 && eval e2 > 0

instance Expr MinMax where
	lit x     = MinMax x
	add e1 e2 = MinMax $ max (eval e1) (eval e2)
	mul e1 e2 = MinMax $ min (eval e1) (eval e2)

instance Expr Mod7 where
	lit x     = Mod7 $ x `mod` 7 
	add e1 e2 = Mod7 $ (eval e1 + eval e2) `mod` 7 
	mul e1 e2 = Mod7 $ (eval e1 * eval e2) `mod` 7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)