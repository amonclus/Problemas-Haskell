data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue first second) = Queue first (x:second)

pop :: Queue a -> Queue a
pop (Queue [] []) = (Queue [] [])
pop (Queue (x:xs) second) = (Queue xs second)
pop (Queue first second) = (Queue (first ++ reverse((init second))) [])

top :: Queue a -> a
top (Queue (x:first) [] ) = x
top (Queue [] second) = last second

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty(_ ) = False

instance Eq a => Eq (Queue a)
    where
        (Queue [] []) == (Queue [] []) = True
        (Queue first1 second1) == (Queue first2 second2)
            |(first1 ++ reverse(second1) == first2 ++ reverse(second2)) = True
            |otherwise = False  


instance Functor Queue where
    fmap func (Queue first second) = Queue (map func first) (map func second)
--Esto define Queue como un functor y usa fmap para poder simular la función map sin alterar la estructura.

--Aplica una traslación a todos los puntos de una cola
translation :: Num b => b -> Queue b -> Queue b
translation num q = fmap (+num) q

cuaLlista :: Queue a -> [a]
cuaLlista (Queue lista1 lista2) = lista1 ++ reverse lista2

instance Applicative Queue where
  pure x = Queue [] [x]
  f <*> q = Queue(cuaLlista f <*> cuaLlista q) []

instance Monad Queue where
  return x = Queue [] [x]
  cua >>= f = Queue (concatMap (cuaLlista . f) (cuaLlista cua)) []

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter func cua = do
  x <- cua
  if func x then
    return x
  else
    create