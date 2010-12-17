module Processor where

import Task
import Queue
import Router

class Processor a where
    runToTime    :: a -> Double -> (a, [CompletedTask])
    acceptTask   :: a -> Task -> a
    step         :: a -> Task -> (a, [CompletedTask])
    {- step should accept the new task and give back all completed ones -}

    step server newTask =
        let (newServer, cTasks) = runToTime server (arrival newTask)
        in (acceptTask newServer newTask, cTasks)

{--------}

data SimpleServer = SimpleServer {
      time      :: Double
    , queue     :: Queue Task
    , current   :: Maybe CurrentTask
    } deriving (Show)

instance Processor SimpleServer where

    acceptTask (SimpleServer time q task) newTask = 
        SimpleServer time (enq q newTask) task

    runToTime ss targetTime =
        runToTime' ss targetTime []


{- idle processor -}    
runToTime' (SimpleServer time q Nothing) targetTime cTasks =
    if empty q
    then (SimpleServer targetTime q Nothing, cTasks)
    else let (task, newQ) = deq q
             newServer    = SimpleServer time newQ (Just (beginTask task))
         in runToTime' newServer targetTime cTasks

{- working processor -}
runToTime' (SimpleServer time q (Just (CurrentTask curTask remaining))) targetTime cTasks =
    let timeToTarget = targetTime - time
    in if remaining > timeToTarget
       then let newCurTask = Just (CurrentTask curTask (remaining - timeToTarget))
            in (SimpleServer targetTime q newCurTask, cTasks)
       else let completionTime = time + remaining
                completedTask  = CompletedTask curTask completionTime
                newServer      = SimpleServer completionTime q Nothing
            in runToTime' newServer targetTime (completedTask : cTasks)


newSingleServer = SimpleServer 0 newQueue Nothing

{--------}

data (Router a) => ServerPair a = ServerPair {
      router      :: a
    , smallServer :: SimpleServer
    , largeServer :: SimpleServer
    } 

instance (Router a) => Processor (ServerPair a) where
    
    runToTime (ServerPair router smallServer largeServer) targetTime = 
        let (newSmallServer, smallCTasks) = runToTime smallServer targetTime
            (newLargeServer, largeCTasks) = runToTime largeServer targetTime
            newServerPair = ServerPair router newSmallServer newLargeServer
        in (newServerPair, smallCTasks ++ largeCTasks)


    acceptTask (ServerPair r a b) task = 
        let serverNum   = route r task
            newR        = updateRouter r task
            newTask     = addToLog task (serverNum, routerStats r) 
        in if serverNum == 0
           then ServerPair newR (acceptTask a newTask) b
           else ServerPair newR a (acceptTask b newTask)


newServerPair router = 
        ServerPair router newSingleServer newSingleServer

{--------}
