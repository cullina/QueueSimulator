import System.Random

data SpacedTask = SpacedTask {
      idS      :: Int
    , spacingS :: Double
    , sizeS    :: Double
    } deriving (Show)

data IncomingTask = IncomingTask {
      idI      :: Int
    , arrivalI :: Double
    , sizeI    :: Double
    } deriving (Show)

data CurrentTask = NoTask | CurrentTask {
      idC        :: Int
    , arrivalC   :: Double
    , sizeC      :: Double
    , remainingC :: Double
    } deriving (Show)

data CompletedTask = CompletedTask {
      idD         :: Int
    , arrivalD    :: Double
    , completionD :: Double
    , sizeD       :: Double
    } deriving (Show)

data Queue a = Queue [a] [a] deriving (Show)

data QueueSystem = QueueSystem {
      time      :: Double
    , incoming  :: [IncomingTask]
    , queue     :: Queue IncomingTask
    , current   :: CurrentTask
    , completed :: [CompletedTask]
}

newQueue = Queue [] []

empty (Queue [] []) = True

empty _ = False

enq (Queue xs ys) x = Queue (x:xs) ys

deq (Queue [] []) = error "Empty"

deq (Queue xs (y:ys)) = (y, Queue xs ys)

deq (Queue xs []) = deq (Queue [] (reverse xs))


exponential lambda unif = -1 * lambda * (log unif)

uniform min range unif = range * unif + min

poissonTaskStream size spacing gen = taskStream (exponential size) (exponential spacing) 0 gen


taskStream spacingDist sizeDist id g0 = 
    let (x, g1) = random g0
        (y, g2) = random g1
        task = SpacedTask id (spacingDist x) (sizeDist y)
    in task : taskStream spacingDist sizeDist (id + 1) g2

convertTask (IncomingTask id arrival size) = CurrentTask id arrival size size


newSystem taskStream = QueueSystem 0 taskStream newQueue NoTask []


{- idle processor -}
stepSystem (QueueSystem time tks@(t:ts) q NoTask completed) =
    if empty q
    then QueueSystem (arrivalI t) ts (enq q t) NoTask completed
    else let (task, newQ) = deq q
         in QueueSystem time tks newQ (convertTask task) completed

{- working processor -}
stepSystem (QueueSystem time tks@(t:ts) q (CurrentTask id arrival size remaining) completed) =
    let timeToArrival = arrivalI t - time
    in if remaining > timeToArrival
       then let newCurTask = (CurrentTask id arrival size (remaining-timeToArrival))
            in QueueSystem (arrivalI t) ts (enq q t) newCurTask completed
       else let completionTime = time + remaining
                newCompleted = (CompletedTask id arrival size completionTime):completed
            in QueueSystem completionTime tks q NoTask newCompleted

timeTransform time ((SpacedTask id spacing size):tasks) = 
    let newTime = time + spacing
    in IncomingTask id newTime size : timeTransform newTime tasks
