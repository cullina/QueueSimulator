module Simulation where

import Processor
import Router
import Estimator
import Accumulator
import Task
import System.Random

{-
system1 :: Double -> Double -> Int -> QueueSystem SimpleServer

system1 load decay = newSystem decay . paretoTaskStream 1 2 load . mkStdGen


system2 :: Double -> Double -> Int -> QueueSystem (ServerPair RoundRobin)

system2 load decay = newSystem decay . paretoTaskStream 0.5 2 load . mkStdGen


system3 :: Double -> Double -> Int -> QueueSystem (ServerPair SizeSplit)

system3 load decay = newSystem decay . paretoTaskStream 0.5 2 load . mkStdGen



simulate numTasks system load decay seed = 
    statistics . take numTasks . runSystem $ system load decay seed
    

massSimulate numSeeds numTasks system load decay seed = 
    let seeds = map (+ seed) [1 .. numSeeds]
    in mergeStats $ map (simulate numTasks system load decay) seeds 
-}

{--------}


data (Processor a) => QueueSystem a = QueueSystem {
      incoming        :: [Task]   
    , processor       :: a
    }


runSystem (QueueSystem (t:ts) processor) =
    let (newProc, cTasks) = step processor t
    in cTasks ++ runSystem (QueueSystem (t:ts) newProc)


exampleProc1 = newServerPair newRoundRobin

exampleProc2 = newServerPair $ SizeSplit $ newRecipEst $ newExpAccum 1

exampleProc3 = newServerPair $ newDirectSplit id 0.1


newEstProc estimator accumulator decay = 
    newServerPair $ SizeSplit $ estimator $ accumulator decay

newDirProc function sensitivity =
    newServerPair $ newDirectSplit function sensitivity

newStream = paretoTaskStream 0.5 2