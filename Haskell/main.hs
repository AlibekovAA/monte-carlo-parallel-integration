module Main where

import Parser (InputData(..), readInputFile)

main :: IO ()
main = do
    inputData <- readInputFile "Haskell/input.txt"
    putStrLn $ "Прочитанные данные:"
    print inputData
