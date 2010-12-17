module Simulation where

import Processor
import Router
import Estimator
import Accumulator
import Task
import System.Random
import Data.List(transpose)

simulate numTasks stream proc seed = 
    statistics . take numTasks . runSystem $ QueueSystem (stream (mkStdGen seed)) proc
   

simulation1 runs len seed = 
    let stream = poissonTaskStream 1 0.5
    in massSimulate runs len stream newSingleServer seed


massSimulate numSeeds numTasks stream proc seed = 
    let seeds = map (+ seed) [0 .. numSeeds - 1]
        runs  = map (simulate numTasks stream proc) seeds 
    in runs


{--------}


data (Processor a) => QueueSystem a = QueueSystem {
      incoming        :: [Task]   
    , processor       :: a
    }


runSystem (QueueSystem (t:ts) processor) =
    let (newProc, cTasks) = step processor t
    in cTasks ++ runSystem (QueueSystem ts newProc)


exampleProc1 = newServerPair newRoundRobin

exampleProc2 = newServerPair $ SizeSplit $ newRecipEst $ newExpAccum 1

exampleProc3 = newServerPair $ newDirectSplit id 0.1


newEstProc estimator accumulator decay = 
    newServerPair $ SizeSplit $ estimator $ accumulator decay

newDirProc function sensitivity =
    newServerPair $ newDirectSplit function sensitivity


newStream size spacing = paretoTaskStream 0.5 2 size spacing
{-
streams seed = map ($ mkStdGen) $ map (poissonTaskStream 1.0 . (*) 0.1) [1 .. 10]
-}