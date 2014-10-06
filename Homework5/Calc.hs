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
  lit :: Integer -> a 
  add :: a -> a  -> a 
  mul :: a -> a  -> a

instance Expr ExprT where
  lit x     = Lit x
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

instance Expr Integer where
  lit x     = x
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit x     = x > 0
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 || e2

instance Expr MinMax where
  lit x     = MinMax $ x
  add e1 e2 = maxMinMax e1 e2
  mul e1 e2 = minMinMax e1 e2

maxMinMax :: MinMax -> MinMax -> MinMax
maxMinMax (MinMax x) (MinMax y) 
  | x > y     = MinMax x
  | otherwise = MinMax y

minMinMax :: MinMax -> MinMax -> MinMax
minMinMax (MinMax x) (MinMax y) 
  | x < y     = MinMax x
  | otherwise = MinMax y

instance Expr Mod7 where
  lit x     = Mod7 $ x `mod` 7 
  add e1 e2 = addMod7 e1 e2
  mul e1 e2 = mulMod7 e1 e2

addMod7 :: Mod7 -> Mod7 -> Mod7
addMod7 (Mod7 x) (Mod7 y) = (Mod7 $ ((x `mod` 7) + (y `mod` 7)) `mod` 7)

mulMod7 :: Mod7 -> Mod7 -> Mod7
mulMod7 (Mod7 x) (Mod7 y) = (Mod7 $ ((x `mod` 7) * (y `mod` 7)) `mod` 7)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)