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
      queueC  :: Queue a
    , lengthC :: Int
    }

newCountedQueue = CountedQueue newQueue 0

enqC (CountedQueue q l) x = CountedQueue (enq q x) (l + 1)

deqC (CountedQueue q l) = let (x, newQ) = deq q
                          in (x, CountedQueue newQ (l - 1))

peekC (CountedQueue q l) = let (x, newQ) = peek q
                           in (x, CountedQueue newQ l)

emptyC = empty . queueC


data SummedQueue a b = SummedQueue {
      queueS  :: Queue a
    , lengthS :: Int
    , sum     :: b
    }

enqS f (SummedQueue q l s) x = SummedQueue (enq q x) (l + 1) (s + f x)

deqS f (SummedQueue q l s) = let (x, newQ) = deq q
                             in (x, SummedQueue newQ (l - 1) (s - f x))

peekS (SummedQueue q l s) = let (x, newQ) = peek q
                            in (x, SummedQueue newQ l s)

emptyS = empty . queueS

newSummedQueue = SummedQueue newQueue 0 0.0