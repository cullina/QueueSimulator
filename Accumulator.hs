module Accumulator where

import Queue

class Accumulator a where
    value  :: a -> Double
    update :: a -> (Double, Double) -> a

{--------}

data SimpleAccum = SimpleAccum {
      accumS :: Double
    , count  :: Double
    }

instance Accumulator SimpleAccum where
    value (SimpleAccum sum count) = 
        sum / count

    update (SimpleAccum sum count) (x, _) = 
        SimpleAccum (sum + x) (count + 1)

newSimpleAccum = SimpleAccum 0 0
        
{--------}

data IntAccum = IntAccum {
      accumI     :: Double
    , lastTimeI  :: Double
    }

instance Accumulator IntAccum where
    value (IntAccum i t1) =
        i / t1

    update (IntAccum i t1) (x, t) =
        IntAccum (i + (t - t1) * x) t

newIntAccum = IntAccum 0 0

{--------}

data SymIntAccum = SymIntAccum {
      accumSI     :: Double
    , lastValueSI :: Double
    , lastTimeSI  :: Double

    } 

instance Accumulator SymIntAccum where
    value (SymIntAccum i x1 t1) =
        i / t1

    update (SymIntAccum i x1 t1) (x, t) =
        SymIntAccum (i + (t - t1) * (x + x1) / 2) t x

newSymIntAccum = SymIntAccum 0 0 0

{--------}

data GeomAccum = GeomAccum {
      decayRateG :: Double
    , accumG     :: Double
    }

instance Accumulator GeomAccum where
    value = accumG

    update (GeomAccum r a) (x, _) =
        GeomAccum r (r * x + (1 - r) * a)

newGeomAccum decay = GeomAccum decay 0

{--------}

data ExpAccum = ExpAccum {
      decayRateE :: Double
    , numerValE  :: Double
    , denomValE  :: Double
    , lastTimeE  :: Double
    }

instance Accumulator ExpAccum where
    value x = numerValE x / denomValE x

    update (ExpAccum r n d oldT) (x, t) =
        let decayFactor = exp (r * (oldT - t))
            newN        = x + decayFactor * n
            newD        = 1 + decayFactor * d 
        in ExpAccum r newN newD t

newExpAccum decay = ExpAccum decay 0 0 0

{--------}

data IntExpAccum = IntExpAccum {
      decayRateIE :: Double
    , accumIE     :: Double
    , lastTimeIE  :: Double
    }

instance Accumulator IntExpAccum where
    value = accumIE

    update (IntExpAccum r a oldT) (x, t) =
        let decayFactor = exp (r * (oldT - t))
            newA        = decayFactor * a + (1 - decayFactor) * x
        in IntExpAccum r newA t

newIntExpAccum decay = IntExpAccum decay 0 0

{--------}

data SymIntExpAccum = SymIntExpAccum {
      decayRateSIE :: Double
    , accumSIE     :: Double
    , lastValueSIE :: Double
    , lastTimeSIE  :: Double
    }

instance Accumulator SymIntExpAccum where
    value = accumSIE 

    update (SymIntExpAccum r a oldX oldT) (x, t) =
        let decayFactor = exp (r * (oldT - t))
            halfDecay   = exp (r * (oldT - t) / 2)
            newA        = decayFactor * a + (1 - halfDecay) * (x + halfDecay * oldX)
        in SymIntExpAccum r newA x t

newSymIntExpAccum decay = SymIntExpAccum decay 0 0 0

{--------}

data FixedSample = FixedSample {
      maxLen   :: Int
    , queueS   :: SummedQueue Double Double
    }

instance Accumulator FixedSample where
    value (FixedSample _ (SummedQueue _ length sum))
        = sum / fromIntegral length

    update (FixedSample maxLen q) (x, _) = 
        let newQ = enqS id q x
            finalQ = if lengthS q > maxLen
                     then snd . deqS id $ newQ
                     else newQ
        in FixedSample maxLen finalQ

newFixedSample length = FixedSample length newSummedQueue

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
    value (FixedInterval _ (SummedQueue _ length sum))
        = sum / fromIntegral length

    update (FixedInterval interval q) pair@(x, t) = 
        let newQ = enqS fst q pair
            removalTime = t - interval
            finalQ = removeOldData removalTime newQ
        in FixedInterval interval finalQ

newFixedInterval interval = FixedInterval interval newSummedQueue