{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage log = case findType log of
  ["I"] -> LogMessage Info (parseFirstInteger log) (parseString log) 
  ["W"] -> LogMessage Warning (parseFirstInteger log) (parseString log)
  ["E"] -> LogMessage (Error (parseFirstInteger log)) (parseSecondInteger log) (parseErrorString log)
  _ -> Unknown log

findType :: String -> [String]
findType log = take 1 (words log)

readIntoInt :: String -> Int
readIntoInt char = read char :: Int 

parseFirstInteger :: String -> Int
parseFirstInteger = readIntoInt . unwords . drop 1 . take 2 . words

parseSecondInteger :: String -> Int
parseSecondInteger = read . unwords . drop 2 . take 3 . words

parseString :: String -> String
parseString = unwords . drop 2 . words

parseErrorString :: String -> String
parseErrorString = unwords . drop 3 . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

newNode:: LogMessage -> MessageTree
newNode a = Node Leaf a Leaf

instance Ord LogMessage where
    (LogMessage _ ts1 _) `compare` (LogMessage _ ts2 _) = ts1 `compare` ts2
    _ `compare` _ = error "Compare error"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf = newNode m
insert m (Node Leaf node rightChild)
	| m < node = Node (newNode m) node rightChild
insert m (Node leftChild node Leaf)	
	| m > node = Node leftChild node (newNode m)
insert m (Node leftChild node rightChild)
	| m < node = Node (insert m leftChild) node rightChild
	| otherwise = Node leftChild node (insert m rightChild)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r  

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getErrorMessage . inOrder . build . filter (severe 50)

severe :: Int -> LogMessage -> Bool
severe minLvl (LogMessage (Error lvl) _ _)
  | lvl > minLvl = True
	| otherwise = False
severe _ _ = False

getErrorMessage :: LogMessage -> String
getErrorMessage (LogMessage _ _ s) = s
getErrorMessage (Unknown s) = s
