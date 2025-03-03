module Main where

import Parser (readInputFile)
import MonteCarlo (timeMonteCarlo)

main :: IO ()
main = do
    inputData <- readInputFile "input.txt"
    putStrLn "Read input data:"
    print inputData

    (result, time) <- timeMonteCarlo inputData
    putStrLn $ "Integral result: " ++ show result
    putStrLn $ "Execution time: " ++ show time ++ " seconds"
