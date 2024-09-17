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

merge :: [Int] -> [Int] -> [Int]
merge [] list = list
merge list [] = list
merge (x1 : list1) (x2: list2)
    | x1 < x2 = x1 : merge list1 (x2:list2)
    | otherwise = x2 : merge list2 (x1:list1)

msort :: [Int] -> [Int]
msort[] = []
msort [x] = [x]
msort list = merge (msort left) (msort right)
    where
        left = take (div (length list) 2) list
        right = drop (div (length list) 2) list    
