module Simulation where

import Processor
import Task
import System.Random

system :: Double -> Int -> (QueueSystem SimpleServer)

system load = newSystem . poissonTaskStream 1 load . mkStdGen

simulate load = statistics . take 100000 . runSystem . system load