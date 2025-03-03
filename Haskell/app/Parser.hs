module Parser (
    InputData(..),
    readInputFile,
    monteCarloFunction
) where

data InputData = InputData {
    variables :: Int,
    bounds :: (Double, Double, Double, Double),
    points :: Int,
    countThreads :: Int
} deriving (Show)

readInputFile :: FilePath -> IO InputData
readInputFile path = do
    content <- lines <$> readFile path
    let vars = read $ parseValue "variables" (head content)
    let boundsData = words $ parseValue "bounds" (content !! 1)
    let [lowX, highX, lowY, highY] = case boundsData of
          [l, h, ll, hh] -> [read l, read h, read ll, read hh]
          _ -> error "Invalid bounds format"
    let pts = read $ parseValue "points" (content !! 2)
    let threads = read $ parseValue "processes" (content !! 3)

    return InputData {
        variables = vars,
        bounds = (lowX, highX, lowY, highY),
        points = pts,
        countThreads = threads
    }

monteCarloFunction :: Double -> Double -> Double
monteCarloFunction x1 x2 = sin x1 + 2 * cos x2

parseValue :: String -> String -> String
parseValue key = drop (length key + 2)
