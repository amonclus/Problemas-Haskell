data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x)     = x
eval1 (Add x y)   = eval1 x + eval1 y
eval1 (Sub x y)   = eval1 x - eval1 y
eval1 (Mul x y)   = eval1 x * eval1 y
eval1 (Div x y)   = eval1 x `div` eval1 y

-- En esta función es posible retornar Nothing, así que hay que cambiar los operadores
eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = operar (+)(eval2 x) (eval2 y)
eval2 (Sub x y) = operar (-)(eval2 x) (eval2 y)
eval2 (Mul x y) = operar (*)(eval2 x) (eval2 y)
eval2 (Div x y)
    |(eval2 y) == (Just 0) = Nothing
    |otherwise = operar (div) (eval2 x) (eval2 y)
        
eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = operar2 (+)(eval3 x) (eval3 y)
eval3 (Sub x y) = operar2 (-)(eval3 x) (eval3 y)
eval3 (Mul x y) = operar2 (*)(eval3 x) (eval3 y)
eval3 (Div x y)
    |(eval3 y) == (Right 0) = Left "div0"
    |otherwise = operar2 (div) (eval3 x) (eval3 y)


operar2 :: (Int -> Int -> Int) -> Either String Int -> Either String Int -> Either String Int
operar2 op (Right x) (Right y) = Right(op x y)
operar2 _ _ _ = Left "div0"


operar :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
operar op (Just x) (Just y) = Just (op x y)
operar _ _ _ = Nothing 