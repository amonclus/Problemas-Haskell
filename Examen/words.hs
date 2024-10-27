import Data.List (sort)

main :: IO()
main = do
    contents <- getContents  -- Reads until EOF
    let palabras = words contents
    let sortedWords = sort palabras
    let wordCounts = countOccurrences sortedWords

    mapM_ printFormatted wordCounts

printFormatted :: (String, Int) -> IO ()
printFormatted (word, count) = putStrLn $ word ++ " " ++ show count

countOccurrences :: [String] -> [(String, Int)]
countOccurrences = foldr insertCount []

insertCount :: String -> [(String, Int)] -> [(String, Int)]
insertCount word [] = [(word, 1)]
insertCount word ((w, n):rest)
    | word == w  = (w, n + 1) : rest
    | otherwise  = (word, 1) : (w, n) : rest