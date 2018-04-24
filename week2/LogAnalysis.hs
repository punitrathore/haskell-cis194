{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("E":mt:ts:msg) -> (LogMessage (Error (read mt::Int)) (read ts::Int) (unwords msg))
  ("I":ts:msg) -> (LogMessage Info (read ts::Int) (unwords msg))
  ("W":ts:msg) -> (LogMessage Warning (read ts::Int) (unwords msg))
  (msg@_) -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ tsNew _) (Node lt lmt@(LogMessage _ ts _) rt) = case tsNew > ts of
  True -> Node lt lmt (insert lm rt)
  False -> Node (insert lm lt) lmt rt

-- Exercise 3
build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

lm1 = (LogMessage Info 1 "abc1")
lm2 = (LogMessage Info 2 "abc2")
lm3 = (LogMessage (Error 100) 3 "abc3")
lm4 = (LogMessage Warning 4 "abc3")

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = inOrder(lt) ++ [lm] ++ inOrder(rt)

-- Exercise 5
isSevereErrorMsg (LogMessage (Error mt) _ _) = mt >= 50
isSevereErrorMsg _ = False

getMessage :: LogMessage -> String
getMessage (Unknown msg) = msg
getMessage (LogMessage _ _ msg) = msg

-- whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . inOrder .  build . (filter isSevereErrorMsg)
