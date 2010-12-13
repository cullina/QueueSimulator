module Router where

import Task
import Estimator

class Router a where
    route        :: a -> Task -> Int
    updateRouter :: a -> Task -> a
    routerStats  :: a -> [Double]

{--------}

data RoundRobin = RoundRobin {
      curServer  :: Int
    , numServers :: Int
    }

instance Router RoundRobin where
    route (RoundRobin a b) t = a

    updateRouter (RoundRobin a b) t = RoundRobin (mod (a + 1) b) b

    routerStats = const []


newRoundRobin = RoundRobin 0 2

{--------}

data Estimator a => SizeSplit a = SizeSplit {
      estimator :: a
    }

instance Estimator a => Router (SizeSplit a) where
    route ss (Task id arrival size) = 
        sgn (size - threshold ss)

    updateRouter (SizeSplit estimator) (Task id arrival size) = 
        SizeSplit (updateEst estimator (size, arrival))

    routerStats ss = [threshold ss]


threshold (SizeSplit e) = workMedian $ parameters e

{--------}

data DirectSplit = DirectSplit {
      conversion  :: Double -> Double
    , sensitivity :: Double
    , difference  :: Double
    }

instance Router DirectSplit where 
    route (DirectSplit conv sens diff) (Task id arrival size) =
        sgn (conv size - diff) 

    updateRouter (DirectSplit conv sens diff) (Task id arrival size) =
        let newSize = conv size
            delta   = sens * newSize
            newDiff = if newSize > diff
                      then diff + delta
                      else diff - delta
        in DirectSplit conv sens newDiff

    routerStats (DirectSplit conv sens diff) = [diff]


newDirectSplit conv sens = DirectSplit conv sens 0

{---------}

sgn x = if x >= 0 then 1 else 0

