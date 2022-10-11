import Data.Char
import Data.List

positions :: [Char] -> [Int]
positions = map (\x -> ord x - 96)

sumsq :: (Num a, Enum a) => a -> a
sumsq n = foldr (\x y -> x ^ 2 + y) 0 [0 .. n]

sum' n = foldr (\x y -> x + y) 0 [0 .. n]

within :: Ord a => [a] -> (a, a) -> [a]
within list (l, u) = filter (\x -> x >= l && x <= u) list

sumrows :: Num a => [[a]] -> [a]
sumrows = map sum

min2 :: Ord a => [a] -> a
min2 list = take 2 (sort list) !! 1

fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys

eapprox :: (Enum a, Fractional a) => a -> a
eapprox n = sum [1 / product [1 .. k] | k <- [0 .. n]]
