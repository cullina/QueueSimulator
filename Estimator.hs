module Estimator where

import Accumulator


class Estimator a where
    updateEst  :: a -> (Double, Double) -> a
    parameters :: a -> (Double, Double)

{--------}

workQuantile (alpha, xMin) p = xMin * recip (1 - p) ** recip (alpha - 1)

workMedian (alpha, xMin) = xMin * 2 ** recip (alpha - 1)


{--------}

data Accumulator a => PairEst a = PairEst {
      accumF   :: Double -> Double
    , convertF :: Double -> Double -> (Double,  Double)
    , basicAcc :: a
    , funcAcc  :: a
    }

instance Accumulator a => Estimator (PairEst a) where

    updateEst (PairEst accumF convertF basicAcc funcAcc) (x, t) =
        let newBasicAcc = update basicAcc (x, t)
            newFuncAcc  = update funcAcc (accumF x, t)
        in PairEst accumF convertF newBasicAcc newFuncAcc
    
    parameters (PairEst accumF convertF basicAcc funcAcc) =
        convertF (value basicAcc) (value funcAcc)

{--------}

newRecipEst newAccum = PairEst recip recipConvert newAccum newAccum

newRootEst newAccum = PairEst sqrt sqrtConvert newAccum newAccum

newSquareEst newAccum = PairEst (\x -> x * x) squareConvert newAccum newAccum

{--------}

recipConvert mean meanRecip = 
    let p     = mean * meanRecip
        alpha = sqrt (p / (p - 1))
        xMin  = mean * (alpha - 1) / alpha
    in (alpha, xMin)

squareConvert mean meanSquare =
    let r     = mean * mean / meanSquare
        s     = sqrt (recip (1 - r))
        alpha = s + 1
        xMin  = mean * s / alpha
    in (alpha, xMin)

sqrtConvert mean meanSqrt =
    let r     = meanSqrt * meanSqrt / mean
        s     = sqrt (recip (1 - r))
        alpha = (s + 1) / 2
        xMin  = mean * (s - 1) / (s + 1)
    in (alpha, xMin)


