module Processor where

import Task
import Queue

class Processor a where
    newProcessor :: a
    step         :: a -> Task -> (a, Maybe CompletedTask)
    {- step should either accept the new task or give back a completed one -}

data (Processor a) => QueueSystem a = QueueSystem {
      incoming  :: [Task]   
    , processor :: a
    }


data SimpleServer = SimpleServer {
      time      :: Double
    , queue     :: Queue Task
    , current   :: Maybe CurrentTask
    }

instance Processor SimpleServer where
    newProcessor = SimpleServer 0 newQueue Nothing

    {- idle processor -}    
    step (SimpleServer time q Nothing) newTask =
        if empty q
        then (SimpleServer (arrival newTask) (enq q newTask) Nothing, Nothing)
        else let (task, newQ) = deq q
             in step (SimpleServer time newQ (Just (beginTask task))) newTask 
    {- working processor -}
    step (SimpleServer time q (Just (CurrentTask curTask remaining))) newTask =
        let timeToArrival = arrival newTask - time
        in if remaining > timeToArrival
           then let newCurTask = (Just (CurrentTask curTask (remaining-timeToArrival)))
                in (SimpleServer (arrival newTask) (enq q newTask) newCurTask, Nothing)
           else let completionTime = time + remaining
                    completedTask = (CompletedTask curTask completionTime)
                in (SimpleServer completionTime q Nothing, Just completedTask)


{-
data ServerPair = {
      smallServer :: SimpleServer
    , largeServer :: SimpleServer
    } 
-}

newSystem taskStream = QueueSystem taskStream newProcessor


runSystem (QueueSystem (t:ts) processor) =
    let (newProc, output) = step processor t
    in case output of
         Nothing    -> runSystem (QueueSystem ts newProc)
         Just cTask -> cTask : runSystem (QueueSystem (t:ts) newProc)