-- (http://www.seas.upenn.edu/~cis194/spring13
-- Problem 1

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log	
import Text.Regex

main :: IO [LogMessage]
main = do 
	contents <- readFile "./sample.log" -- extract String from `IO String` 
	return $ parse contents

-- experiment with lines (String -> [String])
mainCount :: IO Int
mainCount = do 
  contents <- readFile "./sample.log" -- extract String from `IO String` 
  return $ length $ lines contents

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines -- originally used 'words', but that splits on whitespace, not new-line

parseMessage :: String -> LogMessage
parseMessage = convertToMsg . words
               where convertToMsg [] = Unknown ""
                     convertToMsg xxs@("E":mbInt:mbTs:xs) = if (isNumber mbInt && isNumber mbTs) 
                     	                                      then LogMessage (Error $ toInt mbInt) (toInt mbTs) (unwords xs)
                     	                                    else Unknown (unwords xxs)
                     convertToMsg xxs@("I":mbTs:xs)       = if (isNumber mbTs)
                                                              then LogMessage Info (toInt mbTs) (unwords xs)
                                                            else Unknown (unwords xxs)
                     convertToMsg xxs@("W":mbTs:xs)       = if (isNumber mbTs)
                                                              then LogMessage Warning (toInt mbTs) (unwords xs)
                                                            else Unknown (unwords xxs)
                     convertToMsg xs                      = Unknown (unwords xs)

isNumber :: String -> Bool
isNumber = matches' mkNumberRegex

mkNumberRegex :: Regex
mkNumberRegex = mkRegex "^[0-9]+$"               

toInt :: String -> Int
toInt x = read x :: Int

matches' :: Regex -> String -> Bool
matches' r x 
   | matchRegex r x == Nothing = False
   | otherwise                 = True


-- Note that pattern matching is not exhaustive (thanks compiler)
-- Patterns not matched: (LogMessage _ _ _) (Node _ (Unknown _) _)
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = (Node Leaf lm Leaf)
insert lm@(LogMessage _ ts _) (Node l (LogMessage _ treeTs _) r) = if(ts > treeTs) then insert lm l
                                                                   else insert lm r