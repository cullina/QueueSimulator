module Task where

import System.Random

import Data.List(transpose)

data Task = Task {
      id       :: Int
    , arrival  :: Double
    , size     :: Double
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


exponential lambda unif = -1 * lambda * (log unif)


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


beginTask task = CurrentTask task (size task)

delay (CompletedTask (Task id arrival size) completion) = completion - arrival

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