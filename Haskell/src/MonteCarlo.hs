module MonteCarlo (
    monteCarloIntegral,
    timeMonteCarlo
) where

import System.CPUTime
import Parser (InputData(..), monteCarloFunction)
import System.Random (randomRIO)
import Control.Monad (replicateM)

monteCarloIntegral :: InputData -> IO Double
monteCarloIntegral inputData = do
    let (low, high) = bounds inputData
        numPoints = points inputData
        numVars = variables inputData
    points <- replicateM numPoints $ do
        x1 <- randomRIO (low, high)
        x2 <- randomRIO (low, high)
        return (x1, x2)
    let volume = (high - low) ** fromIntegral numVars
        sumValues = sum $ map (\(x1, x2) -> monteCarloFunction x1 x2) points
    return $ (sumValues / fromIntegral numPoints) * volume

timeMonteCarlo :: InputData -> IO (Double, Double)
timeMonteCarlo inputData = do
    start <- getCPUTime
    result <- monteCarloIntegral inputData
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    return (result, diff)
