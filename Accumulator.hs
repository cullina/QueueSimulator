module Accumulator where

import Queue

class Accumulator a where
    accum  :: a -> Double
    update :: a -> (Double, Double) -> a

{--------}

data ExpAccum = ExpAccum {
      decayRate :: Double
    , numerVal  :: Double
    , denomVal  :: Double
    , lastTime  :: Double
    }

instance Accumulator ExpAccum where
    accum x = (numerVal x) / (denomVal x)

    update (ExpAccum r n d oldT) (x, t) =
        let decayFactor = exp (r * (oldT - t))
            newN        = x + decayFactor * n
            newD        = 1 + decayFactor * d 
        in ExpAccum r newN newD t

{--------}

data DumbExp = DumbExp {
      decayRateD :: Double
    , accumVal   :: Double
    , lastTimeD   :: Double
    }

instance Accumulator DumbExp where
    accum = accumVal 

    update (DumbExp r a oldT) (x, t) =
        let decayFactor = exp (r * (oldT - t))
            newA        = decayFactor * a + (1 - decayFactor) * x
        in DumbExp r newA t

{--------}

data FixedSample = FixedSample {
      maxLen   :: Int
    , queueS   :: SummedQueue Double Double
    }

instance Accumulator FixedSample where
    accum (FixedSample _ (SummedQueue _ length sum))
        = sum / fromIntegral length

    update (FixedSample maxLen q) (x, _) = 
        let newQ = enqS id q x
            finalQ = if lengthS q > maxLen
                     then snd . deqS id $ newQ
                     else newQ
        in FixedSample maxLen finalQ

{--------}

data FixedInterval = FixedInterval {
      interval :: Double
    , queueI   :: SummedQueue (Double, Double) Double
    }


removeOldData removalTime q = 
    let ((_, t), newQ) = peekS q
    in if t < removalTime
       then removeOldData removalTime . snd . deqS fst $ newQ
       else newQ

instance Accumulator FixedInterval where
    accum (FixedInterval _ (SummedQueue _ length sum))
        = sum / fromIntegral length

    update (FixedInterval interval q) pair@(x, t) = 
        let newQ = enqS fst q pair
            removalTime = t - interval
            finalQ = removeOldData removalTime newQ
        in FixedInterval interval finalQ