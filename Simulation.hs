module Simulation where

import Processor
import Multiserver
import Router
import Task
import System.Random

system1 :: Double -> Int -> QueueSystem SimpleServer

system1 load = newSystem . paretoTaskStream 1 2 load . mkStdGen


system2 :: Double -> Int -> QueueSystem (ServerPair RoundRobin)

system2 load = newSystem . paretoTaskStream 0.5 2 load . mkStdGen


system3 :: Double -> Int -> QueueSystem (ServerPair SizeSplit)

system3 load = newSystem . paretoTaskStream 0.5 2 load . mkStdGen


simulate numTasks system load seed = 
    statistics . take numTasks . runSystem $ system load seed
    

massSimulate numSeeds numTasks system load seed= 
    let seeds = map (+ seed) [1 .. numSeeds]
    in mergeStats $ map (simulate numTasks system load) seeds 