insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:xs) num 
    | x < num = x:(insert xs num)
    | otherwise = num:x:xs

isort :: [Int] -> [Int]
isort [] = []
isort list = isortRec list []

isortRec :: [Int] -> [Int] -> [Int]
isortRec [] res = res
isortRec (x:xs) res = isortRec xs nueva         --nueva son los elementos ya ordenados
    where 
        nueva = insert res x                    --declaramos nueva y insertamos x en la lista que ya teniamos ordenada


remove :: [Int] -> Int -> [Int]
remove [] num = []
remove (x:xs) num 
    | x == num = xs                             --si nos encontramos el elemento a eliminar, no construimos ninguna lista
    | otherwise = x:(remove xs num)             --con cualquier otro elemento, construimos una lista con el elemento actual, y el resto de la lista con el elemento borrado


ssort :: [Int] -> [Int]
ssort [] = []
ssort list = ssortRec list []

    --primer parámetro: lista sin ordenar;   segundo parametro: lista ya ordenada
ssortRec :: [Int] -> [Int] -> [Int]
ssortRec [] res = res
ssortRec list res = ssortRec nlista nres
    where
        nres = maximum list : res
        nlista = remove list (maximum list)