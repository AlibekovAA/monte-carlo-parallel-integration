module Parser (
    InputData(..),
    readInputFile,
    monteCarloFunction
) where

data InputData = InputData {
    variables :: Int,
    bounds :: (Double, Double),
    points :: Int,
    countThreads :: Int
} deriving (Show)

parseValue :: String -> String -> String
parseValue key str =
    let (_:value) = dropWhile (/= ':') str
    in trim $ tail value
    where trim = dropWhile (== ' ')

readInputFile :: FilePath -> IO InputData
readInputFile path = do
    content <- lines <$> readFile path
    let vars = read $ parseValue "variables" (content !! 0)
    let [low, high] = map read . words $ parseValue "bounds" (content !! 1)
    let pts = read $ parseValue "points" (content !! 2)
    let threads = read $ parseValue "count_threads" (content !! 3)
    return InputData {
        variables = vars,
        bounds = (low, high),
        points = pts,
        countThreads = threads
    }

monteCarloFunction :: Double -> Double -> Double
monteCarloFunction x1 x2 = sin x1 + 2 * cos x2
