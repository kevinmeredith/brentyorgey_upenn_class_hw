{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Calc
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT
                | HasVars Integer
                deriving (Show)

--instance HasVars VarExprT where
--  var ""

--instance Expr VarExprT where
--	lit x = 

--instance HasVars (M.Map String Integer -> Maybe Integer) where
--	var key = M.lookup key

--class Expr a where
--  lit :: Integer -> a 
--  add :: a -> a  -> a 
--  mul :: a -> a  -> a
