module Simulation where

import Processor
import Multiserver
import Router
import Task
import System.Random

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