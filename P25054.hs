{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

myLength :: [Int] -> Int
myLength [] = 0
myLength(x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum (first : rest)
    | null rest || first > y = first            --Si solo hay un elemento o si el primero es el máximo entre el resto de elementos, se retorna el primero
    | otherwise = y                             --En caso contrario, se retorna el máximo del resto
    where y = myMaximum rest                    --Aquí calculamos el máxmio del resto

average :: [Int] -> Float
average list = fromIntegral(sum(list)) / fromIntegral(myLength(list))           --La función fromIntegral nos permite usar los enteros de la suma y longitud y convertirlos a float


buildPalindrome :: [Int] -> [Int]
buildPalindrome list = reverse(list) ++ list

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x:xs) d
    | elem x d = remove xs d
    | otherwise = x: remove xs d    

flatten :: [[Int]] -> [Int] 
flatten [] = []
flatten (x: xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs)
  |even x = (a, x : b)
  |otherwise = (x : a, b)
  where
    (a, b) = oddsNevens xs

primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x = reverse (primeDivisorsRec x x)

primeDivisorsRec :: Int -> Int -> [Int]
primeDivisorsRec _ 0 = []
primeDivisorsRec x y
  |mod x y == 0 && isPrime y = y:primeDivisorsRec x (y - 1)
  |otherwise = primeDivisorsRec x (y - 1)

isPrime :: Int -> Bool
isPrime n
  |n < 2 = False
  |otherwise = isPrimeRec n (n - 1)

isPrimeRec :: Int -> Int -> Bool
isPrimeRec x p
  |p == 1 = True
  |mod x p == 0 = False
  |otherwise = isPrimeRec x (p - 1)