quango :: a -> [a]
quango a = [a]

tango :: Num p1 => (a, b) -> p2 -> p1
tango (x, y) n = 3 + 3

twice :: (a -> a) -> a -> a
twice f x = f (f (x))

dingo :: (a, a) -> [a]
dingo (x, y) = [x, y]

bighead :: (Ord a, Num b) => [a] -> b
bighead list = undefined

mango x y z = x * y + z - 42

bingo :: a -> a
bingo x = x

thesame :: (Eq a) => [(a, a)] -> [(a, a)]
thesame = undefined

-- [(+), (*), (+), (-)] :: Num a => [(a -> a -> a)]

map' :: (a -> b) -> [a] -> [b]
map' = undefined

something :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
something a b (c, d) | c < d && a == b = c

-- madras (f, x, y) = f (f x x) y
madras f x y = f (f x x) y