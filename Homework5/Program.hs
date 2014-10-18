{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Program (compile, convert) where

import StackVM
import Text.Regex
import Control.Applicative

---- The various expressions our VM understands.
--data StackExp = PushI Integer
--              | PushB Bool
--              | Add
--              | Mul
--              | And
--              | Or
--                deriving Show

--type Stack   = [StackVal]
--type Program = [StackExp]


compile :: String -> Maybe Program
compile = (compile' . words)
   where compile' []                      = Just []
         compile' ("PushI": x : xs)       = if isInteger x then Just ((:) (PushI (read x :: Integer))) <*> compile' xs
         	                                else Nothing
         compile' ("PushB": "True" :  xs) = (PushB True :)  <$> compile' xs
         compile' ("PushB": "False" : xs) = (PushB False :) <$> compile' xs
         compile' ("Add":xs)              = (Add :)         <$> compile' xs
         compile' ("Mul":xs)              = (Mul :)         <$> compile' xs
         compile' ("And":xs)              = (And :)         <$> compile' xs
         compile' ("Or" :xs)              = (Or :)          <$> compile' xs
         compile' _                       = Nothing

-- Expects a String containing an individual command
-- If it's valid, return Just; otherwise Nothing
convert :: String -> Maybe StackExp
convert = (compile' . words)
  where compile' []                      = Nothing 
        compile' ("PushI": x : [])       = if isInteger x then Just $ PushI (read x :: Integer) else Nothing
        compile' ("PushB": "True" : [])  = Just $ PushB True
        compile' ("PushB": "False" : []) = Just $ PushB False
        compile' ("Add":[])              = Just Add
        compile' ("Mul":[])              = Just Mul
        compile' ("And":[])              = Just And
        compile' ("Or" :[])              = Just Or
        compile' _                       = Nothing

isInteger :: String -> Bool
isInteger = matches' mkIntegerRegex

mkIntegerRegex :: Regex
mkIntegerRegex = mkRegex "^[0-9]+$"        

matches' :: Regex -> String -> Bool
matches' r x 
   | matchRegex r x == Nothing = False
   | otherwise                 = True