flatten :: [[Int]] -> [Int]
flatten list = foldr (++) [] list

myLength :: String -> Int
myLength list = length list

myReverse :: [Int] -> [Int]
myReverse list = foldr (\x y -> y ++ [x]) [] list       ---dada una x(elemento de la lista) y una y(lista vacia), recorre des de la derecha y va añadiendo elementos a una lista vacia
                                                    
countIn :: [[Int]] -> Int -> [Int]
countIn list x = map (myCount x) list                   --Como map aplica a cada elemento de la lista de listas, se implicita un segundo param de una lista simple
    where
        myCount num subList = length(filter (== num) subList)

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') (dropWhile (==' ') s)      --Primero borra todos los espacios de delante y luego elimina todo lo que venga despues del espacio de la primera palabra
--a