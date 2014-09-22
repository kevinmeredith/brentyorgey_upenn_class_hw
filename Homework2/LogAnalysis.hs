-- (http://www.seas.upenn.edu/~cis194/spring13
-- Problem 1

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log	
import Text.Regex
import Data.Function (on)
import Data.List (sortBy)

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
insert logMsg Leaf = (Node Leaf logMsg Leaf)
insert logMsg@(LogMessage _ logMsgTimeStamp _) (Node l treeMsg@(LogMessage _ treeTimeStamp _) r) = if(logMsgTimeStamp > treeTimeStamp) then (Node (insert logMsg l) treeMsg r)
                                                                                           else (Node l treeMsg (insert logMsg r)) 

--we can build a complete MessageTree from a list of messages.
build :: [LogMessage] -> MessageTree
build = foldr (insert) Leaf                                                                   

-- which takes a sorted MessageTree and produces a list of all the
-- LogMessages it contains, sorted by timestamp from smallest to biggest.
inOrder :: MessageTree -> [LogMessage]
inOrder = sortBy (compare `on` timeStamp) . extract

timeStamp :: LogMessage -> Int
timeStamp (LogMessage _ x _) = x

extract :: MessageTree -> [LogMessage]
extract Leaf = []
extract (Node left msg right) = msg : (extract left) ++ (extract right)

--We have decided that “relevant” means “errors with a severity of at least 50”.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = ((inOrder . build) msgs) >>= getErrorWithSevAtLeast50 

getErrorWithSevAtLeast50 :: LogMessage -> [String]
getErrorWithSevAtLeast50 (LogMessage (Error sev) _ msg) = if (sev >= 50) then [msg] else []
getErrorWithSevAtLeast50 _ = []

testSampleLog :: IO [String]
testSampleLog = check $ messageList >>= f
    where messageList = testParse parse 100 "sample.log"
          check x     = if (length x \= 3) then error("failed!") else "success"


f :: [LogMessage] -> IO [String]
f x = return $ whatWentWrong x 