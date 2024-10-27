ones :: [Integer] 
ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = 0 : concatMap (\n -> [n, -n]) [1..]