module Queue where

data Queue a = Queue [a] [a] deriving (Show)


newQueue = Queue [] []

empty (Queue [] []) = True

empty _ = False

enq (Queue xs ys) x = Queue (x:xs) ys

deq (Queue [] []) = error "Empty"

deq (Queue xs (y:ys)) = (y, Queue xs ys)

deq (Queue xs []) = deq (Queue [] (reverse xs))

peek (Queue [] []) = error "Empty"

peek (Queue xs ys@(y:_)) = (y, Queue xs ys)

peek (Queue xs []) = peek (Queue [] (reverse xs))




data CountedQueue a = CountedQueue {
      queue  :: Queue a
    , length :: Int
    }

newCountedQueue = CountedQueue newQueue 0

enqC (CountedQueue q l) x = CountedQueue (enq q x) (l + 1)

deqC (CountedQueue q l) = let (a, b) = deq q
                          in (a, CountedQueue b (l - 1))

peekC (CountedQueue q l) = let (a, b) = peek q
                           in (a, CountedQueue b l)


emptyC = empty . queue