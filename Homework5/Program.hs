{-# LANGUAGE TypeSynonymInstances #-}
module Program (compile) where

import StackVM
import Text.Regex

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

--compile :: String -> Maybe Program
--compile [] = Nothing
--compile [] 

convert :: String -> Maybe StackExp
convert = (convert' . words)
  where convert' [] = Nothing :: [String] -> Maybe StackExp
        convert' ("PushI": x : [])       = if isInteger x then Just $ PushI (read x :: Integer) else Nothing
        convert' ("PushB": "True" : [])  = Just True
        convert' ("PushB": "False" : []) = Just False
        convert' ("Add":[])              = Just Add
        convert' ("Mul":[])              = Just Mul
        convert' ("And":[])              = Just And
        convert' ("Or" :[])              = Just Or
        convert' _                       = Nothing

isInteger :: String -> Bool
isInteger = matches' mkIntegerRegex

mkIntegerRegex :: Regex
mkIntegerRegex = mkRegex "^[1-9]+[0-9]*$"        