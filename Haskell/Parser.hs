module Parser (
    InputData(..),
    readInputFile
) where

data InputData = InputData {
    function :: String,
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
    let func = parseValue "function" (content !! 0)
    let vars = read $ parseValue "variables" (content !! 1)
    let [low, high] = map read . words $ parseValue "bounds" (content !! 2)
    let pts = read $ parseValue "points" (content !! 3)
    let threads = read $ parseValue "count_threads" (content !! 4)
    return InputData {
        function = func,
        variables = vars,
        bounds = (low, high),
        points = pts,
        countThreads = threads
    }
