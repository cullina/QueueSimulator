module Queue where

data Queue a = Queue [a] [a] deriving (Show)


newQueue = Queue [] []

empty (Queue [] []) = True

empty _ = False

enq (Queue xs ys) x = Queue (x:xs) ys

deq (Queue [] []) = error "Empty"

deq (Queue xs (y:ys)) = (y, Queue xs ys)

deq (Queue xs []) = deq (Queue [] (reverse xs))
