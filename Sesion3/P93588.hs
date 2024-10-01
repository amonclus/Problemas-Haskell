myMap :: (a -> b) -> [a] -> [b]
myMap op list = [op x | x <- list]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter op list = [x | x <- list, op x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op list1 list2 = [op x y | (x,y) <- zip list1 list2]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify list1 list2 = [(x,y) | x <- list1, y <- list2, (mod x y) == 0]

factors :: Int -> [Int]
factors x = [n | n <- [1..x], (mod x n) == 0]