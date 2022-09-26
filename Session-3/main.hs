onlyTwo :: [a] -> Bool
onlyTwo [x, y] = True
onlyTwo _ = False

alldots :: Num t => [(t, t)] -> [(t, t)] -> [t]
alldots as bs = [dot a b | a <- as, b <- bs]
  where
    dot (a, b) (c, d) = a * c + b * d

pyt :: Int -> [(Int, Int, Int)]
pyt k = [(a, b, c) | a <- [0 .. k], b <- [a .. k], c <- [b .. k], a <= b && b < c && a ^ 2 + b ^ 2 == c ^ 2]

-- pyt k = [(a, b, c) | c <- [1 .. k], b <- [1 .. c], a <- [1 .. b], a * a + b * b == c * c]

sevens :: Integral a => a -> [a]
sevens n = [x | x <- [1 .. n], mod x 7 == 0]

plonk :: Num a => a -> a -> a -> a
plonk = \x -> \y -> \z -> x + y + z

flop :: [(a, b)] -> [(b, a)]
flop pairs = [(y, x) | (x, y) <- pairs]

dupli :: [a] -> [a]
dupli xs = concat [[x, x] | x <- xs]

isperfect :: Integral a => a -> Bool
isperfect n = sum (nontrivialfactors n) == n
  where
    nontrivialfactors n = [x | x <- [1 .. (n - 1)], n `mod` x == 0]

bighead :: (Ord a, Num b) => [a] -> b
bighead xs = sum [1 | x <- xs, x > n]
  where
    n = head xs

bighead' :: Ord a => [a] -> Int
bighead' xs = length [x | x <- xs, x > head xs]

sums :: (Num a, Enum a) => a -> a -> [a]
sums m n = [x + y | x <- [1 .. m], y <- [1 .. n]]

sums' :: (Num a, Enum a) => a -> a -> [a]
sums' m n = concat [[x + y | y <- [1 .. n]] | x <- [1 .. m]]