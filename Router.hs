module Router where

import Task
import Estimator

class Router a where
    route        :: a -> Task -> Int
    updateRouter :: a -> Task -> a

{--------}

data RoundRobin = RoundRobin {
      curServer  :: Int
    , numServers :: Int
    }

instance Router RoundRobin where
    route (RoundRobin a b) t = a

    updateRouter (RoundRobin a b) t = RoundRobin (mod (a + 1) b) b


{--------}

data Estimator a => SizeSplit a = SizeSplit {
      estimator :: a
    }

instance Estimator a => Router (SizeSplit a) where
    route ss (Task id arrival size) = 
        if size > threshold ss then 1 else 0

    updateRouter (SizeSplit estimator) (Task id arrival size) = 
        SizeSplit (updateEst estimator (size, arrival))

{--------}

threshold (SizeSplit e) = workMedian $ parameters e
