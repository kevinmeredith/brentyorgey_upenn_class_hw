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