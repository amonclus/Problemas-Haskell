myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a                                  --caso base: Cuando recorre todo el vector, retorna el num que queda
myFoldl op n (x:xs) = myFoldl op (op n x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ a [] = a
myFoldr op n (x:xs) = op x (myFoldr op n xs)

myIterate :: (a -> a) -> a -> [a]
myIterate op x = [x]++myIterate op (op x)