calcBMI :: Double -> Double -> Double
calcBMI m h = m / (h*h)


getBMI :: Double -> Double -> String
getBMI m h
    | x < 18 = "underweight"
    | 18 <= x && x < 25 = "normal weight"
    | 25 <= x && x < 30 = "overweight"
    | 30 <= x && x < 40 = "obese"
    | otherwise = "severely obese"
    where x = calcBMI m h

    
main :: IO()
main = do
    line <-getLine
    if (head line) == '*' then
        return()
    else do
        let w = words line
        let name = w !! 0
        let weight = read (w !! 1) :: Double
        let height = read (w !! 2) :: Double
        let res = getBMI weight height
        let out = (name ++ ": " ++ res)
        putStrLn out
        main
