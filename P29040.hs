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
