{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

data FailableMsgType = Failure | OK MessageType

parse :: String -> [LogMessage]
parse x = [parseMessage line | line <- lines x]

parseMessage :: String -> LogMessage
parseMessage x = parseWhole (words x)

parseWhole :: [String] -> LogMessage
parseWhole x = case head x of
  "I" -> LogMessage Info (read (x !! 1) :: TimeStamp) (unwords (drop 2 x))
  "E" -> LogMessage (Error (read (x !! 1) :: Int)) (read (x !! 2) :: TimeStamp) (unwords (drop 3 x))
  "W" -> LogMessage Warning (read (x !! 1) :: TimeStamp) (unwords (drop 2 x))
  _ -> Unknown (unwords x)

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg tree = case logMsg of
  Unknown _ -> tree
  msg@(LogMessage _ logTime _) -> case tree of
    Leaf -> Node Leaf logMsg Leaf
    node@(Node left nodeLog right) ->
      let time = nodeTime node
       in if logTime > time
            then Node left nodeLog (insert msg right)
            else Node (insert msg left) nodeLog right

nodeTime :: MessageTree -> TimeStamp
nodeTime x = case x of
  Leaf -> 0
  (Node _ logMsg _) -> case logMsg of
    (Unknown _) -> 0
    (LogMessage _ time _) -> time

message :: LogMessage -> String
message (LogMessage _ _ msg) = msg
message _ = ""

build :: [LogMessage] -> MessageTree
build x
  | null x = Leaf
  | otherwise = insert (head x) (build (tail x))

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left value right) = inOrder left ++ [value] ++ inOrder right
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = logFilter (inOrder (build x))

logFilter :: [LogMessage] -> [String]
logFilter [] = []
logFilter (x : xs) = if isRelevant x then message x : logFilter xs else logFilter xs

isRelevant :: LogMessage -> Bool
isRelevant x = case x of
  (Unknown _) -> False
  (LogMessage (Error sev) _ _) -> sev >= 50
  LogMessage {} -> False
