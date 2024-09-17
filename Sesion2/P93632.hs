eql :: [Int] -> [Int] -> Bool
eql list1 list2 = list1 == list2

prod :: [Int] -> Int
prod list = foldl (*) 1 list

prodOfEvens :: [Int] -> Int
prodOfEvens list = prod (filter even list)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = foldl (+) 0(zipWith (*) xs ys)