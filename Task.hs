module Task where

import System.Random

import Data.List(transpose, foldl')

data Task = Task {
      idNum      :: Int
    , arrival    :: Double
    , size       :: Double
    , routingLog :: [(Int, [Double])]
    } deriving (Show)

data CurrentTask = CurrentTask {
      curTask    :: Task
    , remaining  :: Double
    } deriving (Show)

data CompletedTask = CompletedTask {
      compTask   :: Task
    , completion :: Double
    } deriving (Show)



addToLog (Task id arrival size routingLog) entry = 
    Task id arrival size (entry : routingLog) 

beginTask task = CurrentTask task (size task)

{---------}

uniform min range unif = range * unif + min


exponential mean unif = -1 * mean * log unif


pareto recipAlpha min unif = min / (unif ** recipAlpha)


paretoFromMean recipAlpha mean = pareto recipAlpha ((1 - recipAlpha) * mean) 

{---------}

poissonTaskStream spacing size = 
    taskStream (exponential spacing) (exponential size) 0 0

paretoTaskStream spacing alpha size = 
    taskStream (exponential spacing) (paretoFromMean (1 / alpha) size) 0 0


taskStream spacingDist sizeDist id time g0 = 
    let (x, g1) = random g0
        (y, g2) = random g1
        spacing = spacingDist x
        size    = sizeDist y
        task    = Task id time size []
    in task : taskStream spacingDist sizeDist (id + 1) (time + spacing) g2


{- ids will be incorrect -}
piecewiseTaskStream spD1 szD1 spD2 szD2 iDist id time state interval g0 =  
    let (x, g1) = random g0
        (y, g2) = random g1
    in if time > interval
       then let newInterval = interval + iDist x
                newState    = not state
                spacing     = if state then spD1 y else spD2 y
                newTime     = interval + spacing
            in piecewiseTaskStream spD1 szD1 spD2 szD2 iDist id newTime newState newInterval g2
       else let (spacing, size) = if state
                                  then (spD1 x, szD1 y)
                                  else (spD2 x, szD2 y)
                task            = Task id time size []
                newTime         = time + spacing
            in task : piecewiseTaskStream spD1 szD1 spD2 szD2 iDist (id + 1) newTime state interval g2

{---------}

data TaskStats = TaskStats !Double !Double !Double !Double !Int

instance Show TaskStats where
    show (TaskStats a b c d e) = 
        let digits      = 6
            len         = fromIntegral e
            size        = take digits $ show $ a / len
            delay       = take digits $ show $ b / len
            scaledDelay = take digits $ show $ b / a
            slowdown    = take digits $ show $ c / len
            rate        = take digits $ show $ d / len
        in size ++ " " ++ delay ++ " " ++ scaledDelay ++ " " ++ 
           slowdown ++ " " ++ rate ++ " " ++ show e ++ "\n"
            


delay (CompletedTask (Task id arrival size routingLog) completion) = completion - arrival

compSize = size . compTask

slowdown t = delay t / compSize t

taskStats t = TaskStats (compSize t) (delay t) (slowdown t) (completion t) 0

sumStats (TaskStats a b c d e) (TaskStats f g h i _) = 
    TaskStats (a + f) (b + g) (c + h) (max d i) (e + 1)

statistics = foldl' sumStats (TaskStats 0 0 0 0 0) . map taskStats


showWLines list = concatMap ((++ "\n") . show) list
