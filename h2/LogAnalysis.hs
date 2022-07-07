{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage ll =
    let ws = words ll
        indicater = head ws
        pit cnt = read . head . drop cnt $ ws
        ct cnt = unwords . drop cnt $ ws
    in
        case indicater of
            "I" -> LogMessage Info (pit 1) (ct 2)
            "W" -> LogMessage Warning (pit 1) (ct 2)
            "E" -> LogMessage (Error (pit 1)) (pit 2) (ct 3)
            _ -> Unknown ll

parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert m (Leaf) = Node Leaf m Leaf
insert m@(LogMessage _ time _) (Node left root@(LogMessage _ rt _) right)
    | time < rt = Node (insert m left) root right
    | otherwise = Node left root (insert m right)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

inOrder :: MessageTree -> [LogMessage]
inOrder node = doit node [] where
    doit Leaf now = now
    doit (Node left root right) now = doit left (root:doit right now)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map ct . filter error . inOrder . build
    where error (LogMessage (Error n) _ _) = n >= 50
          error _ = False
          ct (LogMessage _ _ c) = c
          ct _ = ""
