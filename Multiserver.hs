module Multiserver where

import Router
import Processor



data (Router a) => ServerPair a = ServerPair {
      router      :: a
    , smallServer :: SimpleServer
    , largeServer :: SimpleServer
    } 

instance (Router a) => Processor (ServerPair a) where
    newProcessor = ServerPair (newRouter 1) newProcessor newProcessor

    runToTime sp@(ServerPair r a b) targetTime = 
        runToTime' sp targetTime 0

    acceptTask (ServerPair r a b) task = 
        let serverNum = route r task
            newR      = updateRouter r task
        in if serverNum == 0
           then ServerPair newR (acceptTask a task) b
           else ServerPair newR a (acceptTask b task)



runToTime' sp@(ServerPair r a b) targetTime 0 = 
    let (newA, cTask) = runToTime a targetTime
        newSP         = ServerPair r newA b   
    in case cTask of
         Nothing -> runToTime' newSP targetTime 1
         Just _  -> (newSP, cTask)

runToTime' sp@(ServerPair r a b) targetTime 1 = 
    let (newB, cTask) = runToTime b targetTime
        newSP         = ServerPair r a newB   
    in case cTask of
         Nothing -> runToTime' newSP targetTime 2
         Just _  -> (newSP, cTask)

runToTime' sp@(ServerPair r a b) targetTime 2 = (sp, Nothing)
