fizzBuzz :: [Either Int String]
fizzBuzz = map (func) [0..]         --We apply the function "func" to each element of the list
    where 
    func :: Int -> Either Int String
    func n 
        |mod n 5 == 0 && mod n 3 == 0 = Right "FizzBuzz"
        |mod n 3 == 0 = Right "Fizz"
        |mod n 5 == 0 = Right "Buzz"
        |otherwise = Left n
       
    