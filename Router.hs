module Router where

import Task


class Router a where
    newRouter    :: Double -> a
    route        :: a -> Task -> Int
    updateRouter :: a -> Task -> a

data RoundRobin = RoundRobin {
      curServer  :: Int
    , numServers :: Int
    }

instance Router RoundRobin where
    newRouter _ = RoundRobin 0 2

    route (RoundRobin a b) t = a

    updateRouter (RoundRobin a b) t = RoundRobin (mod (a + 1) b) b



data PairRouter = PairRouter {
      decayRate         :: Double
    , meanLogEst        :: Double
    , estimateTimestamp :: Double
    }

instance Router PairRouter where
    newRouter decay = PairRouter decay 0 0

    route (PairRouter decayRate meanLogEst estimateAge) (Task id arrival size) = 
        let logSize      = log size
            medianLogEst = (log 2) * meanLogEst
            serverNum    = if logSize > medianLogEst then 1 else 0
        in serverNum

    updateRouter (PairRouter decayRate meanLogEst estimateAge) (Task id arrival size) = 
        let logSize      = log size
            decayFactor  = exp (decayRate * (estimateAge - arrival))
            newMean      = decayFactor * meanLogEst + (1 - decayFactor) * logSize
            newRouter    = PairRouter decayRate newMean arrival
        in newRouter


