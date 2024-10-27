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