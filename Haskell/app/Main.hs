module Main where

import Parser (readInputFile)
import MonteCarlo (timeMonteCarlo)

main :: IO ()
main = do
    inputData <- readInputFile "input.txt"
    putStrLn $ "Прочитанные данные:"
    print inputData

    (result, time) <- timeMonteCarlo inputData
    putStrLn $ "Результат интеграла: " ++ show result
    putStrLn $ "Время выполнения: " ++ show time ++ " секунд"
