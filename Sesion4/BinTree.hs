data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int 
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

equal :: Eq a => Tree a -> Tree a -> Bool       --Eq means that the type of a must be equatable
equal (Node _ l1 r1) Empty = False
equal Empty (Node _ l1 r1)  = False
equal Empty Empty = True
equal (Node x l1 r1) (Node y l2 r2)
    | x == y = (equal l1 l2) && (equal r1 r2)
    | otherwise= False

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic (Node _ l1 r1) Empty = False
isomorphic Empty (Node _ l1 r1)  = False
isomorphic Empty Empty = True
isomorphic (Node x l1 r1) (Node y l2 r2)
    |x == y = ((isomorphic l1 l2) && (isomorphic r1 r2)) || ((isomorphic l1 r2) && (isomorphic l2 r1))
    |otherwise = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = x : preOrder l ++ preOrder r

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

breadthFirst :: Tree a -> [a]
breadthFirst t = breadthFirst' [t]           --Start with a list containing the root
    where
        breadthFirst' [] = []
        breadthFirst' (Empty:xs) = breadthFirst' xs
        breadthFirst' ((Node x l r):xs) = x:breadthFirst'(xs ++ [l, r])

build :: Eq a => [a] -> [a] -> Tree a
build _ [] = Empty
build [] _ = Empty
build (nodo:preordre) inordre = Node nodo (build preordreEsquerra inordreEsquerra) (build preordreDret inordreDret)
  where
    inordreEsquerra = takeWhile (/=nodo) inordre
    inordreDret = tail$dropWhile (/=nodo) inordre
    preordreEsquerra = take (length inordreEsquerra) preordre
    preordreDret = drop (length inordreEsquerra) preordre

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ t1 Empty = t1
overlap _ Empty t2 = t2
overlap operacio (Node x fex fdx) (Node y fey fdy) = Node (x `operacio` y) (overlap operacio fex fey) (overlap operacio fdx fdy)