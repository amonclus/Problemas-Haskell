sayHi :: String -> IO()
sayHi (x:xs) 
    | x == 'A' || x == 'a' = putStrLn "Hello!"
    |otherwise = putStrLn "Bye!"


main :: IO ()
main = do
    line <- getLine
    sayHi (line)