module Task where

import System.Random

import Data.List(transpose)

data Task = Task {
      idNum      :: Int
    , arrival    :: Double
    , size       :: Double
    , routingLog :: [(Int, Double)]
    } deriving (Show)

data CurrentTask = CurrentTask {
      curTask    :: Task
    , remaining  :: Double
    } deriving (Show)

data CompletedTask = CompletedTask {
      compTask   :: Task
    , completion :: Double
    } deriving (Show)



uniform min range unif = range * unif + min


exponential mean unif = -1 * mean * (log unif)


pareto recipAlpha min unif = min / (unif ** recipAlpha)


paretoFromMean recipAlpha mean = pareto recipAlpha ((1 - recipAlpha) * mean) 



poissonTaskStream spacing size gen = 
    taskStream (exponential spacing) (exponential size) 0 0 gen

paretoTaskStream spacing alpha size gen = 
    taskStream (exponential spacing) (paretoFromMean (1 / alpha) size) 0 0 gen


taskStream spacingDist sizeDist id time g0 = 
    let (x, g1) = random g0
        (y, g2) = random g1
        spacing = spacingDist x
        size    = sizeDist y
        task    = Task id time size
    in task : taskStream spacingDist sizeDist (id + 1) (time + spacing) g2


{- ids will be incorrect -}
piecewiseTaskStream spD1 szD1 spD2 szD2 iDist id time state interval g0 =  
    let (x, g1) = random g0
        (y, g2) = random g1
    in if time > interval
       then let newInterval = interval + (iDist x)
                newState    = not state
                spacing     = if state then spD1 y else spD2 y
                newTime     = interval + spacing
            in piecewiseTaskStream spD1 szD1 spD2 szD2 iDist id newTime newState newInterval g2
       else let (spacing, size) = if state
                                  then (spD1 x, szD1 y)
                                  else (spD2 x, szD2 y)
                task            = Task id time size
                newTime         = time + spacing
            in task : piecewiseTaskStream spD1 szD1 spD2 szD2 iDist (id + 1) newTime state interval g2




beginTask task = CurrentTask task (size task)

delay (CompletedTask (Task id arrival size routingLog) completion) = completion - arrival

compSize = size . compTask

slowdown t = (delay t) / (compSize t)

sumStat f completed = sum (map f completed)


statistics completed = 
    let l     = length completed
        len   = fromIntegral l
        stats = map (\f -> sumStat f completed) [compSize, delay, slowdown]
    in  map (/ len) stats

mergeStats statList = 
    let l = fromIntegral $ length statList
    in map (\x -> (sum x) / l) (transpose statList) 