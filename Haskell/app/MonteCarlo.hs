module MonteCarlo (
    monteCarloIntegral,
    timeMonteCarlo
) where

import Prelude
import Control.Monad (replicateM, mapM)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent (getNumCapabilities)
import Parser (InputData(..), monteCarloFunction)

monteCarloIntegral :: InputData -> IO Double
monteCarloIntegral inputData = do
    numCapabilities <- getNumCapabilities

    let numProcessesRequested = countThreads inputData

    let numProcesses = min numProcessesRequested numCapabilities

    let (lowX, highX, lowY, highY) = bounds inputData
        numPoints = points inputData
        numVars = variables inputData

    let pointsPerProcess = numPoints `div` numProcesses
        remainder = numPoints `mod` numProcesses
        chunks = replicate (numProcesses - 1) pointsPerProcess ++ [pointsPerProcess + remainder]

    let generatePointsForProcess chunk = replicateM chunk $ do
            x1 <- randomRIO (lowX, highX)
            x2 <- randomRIO (lowY, highY)
            return (x1, x2)

    let calculateForProcess chunk = do
            points <- generatePointsForProcess chunk
            let sumValues = sum $ map (uncurry monteCarloFunction) points
            return sumValues

    results <- mapM calculateForProcess chunks
    let evaluatedResults = results `using` parList rdeepseq

    let totalSum = sum evaluatedResults
        volume = (highX - lowX) * (highY - lowY)
        result = (totalSum / fromIntegral numPoints) * volume
    return result

timeMonteCarlo :: InputData -> IO (Double, Double)
timeMonteCarlo inputData = do
    start <- getCurrentTime
    result <- monteCarloIntegral inputData
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    return (result, diff)
