absValue :: Int -> Int
absValue n
    | n > 0 = n
    | otherwise = -n 

power :: Int -> Int -> Int 
power _ 0 = 1
power x n
    | even n = y * y
    | otherwise = x*y*y

    where
        y = power x n_halved 
        n_halved = n `div` 2


isPrime :: Int -> Bool
isPrime n
  |n < 2 = False
  |otherwise = isPrimeRec n (n - 1)

isPrimeRec :: Int -> Int -> Bool
isPrimeRec x p
  |p == 1 = True
  |mod x p == 0 = False
  |otherwise = isPrimeRec x (p - 1)


slowFib :: Int -> Int
slowFib n
    | n <= 1 = n
    |otherwise = slowFib(n-1) + slowFib(n-2)

quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n = quickFibRec n 0 1 2

quickFibRec :: Int -> Int -> Int -> Int -> Int
quickFibRec busqueda primero segundo iteracion
  |iteracion == busqueda = primero + segundo
  |otherwise = quickFibRec busqueda segundo (primero + segundo) (iteracion + 1)