replicate' :: a -> Int -> [a]
replicate' _ 0 = []
replicate' x n = x : replicate' x (n - 1)

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x : _ : rest) = x : improve rest

improve' :: [a] -> [a]
improve' [] = error "Undefined for empty lists"
improve' [x] = error "Undefined for lists with less than two elements"
improve' (x : _ : rest)
  | not (null rest) = x : improve rest
  | otherwise = [x]

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

last' :: [a] -> a
last' [] = undefined
last' [x] = x
last' (_ : xs) = last' xs

wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x : rest)
  | x == h = (x : nextList) : restList
  | otherwise = [x] : wrapup rest
  where
    nextList : restList = wrapup rest
    (h : t) = nextList

rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle [x] = [(x, 1)]
rle (x : rest)
  | x == f = (x, l + 1) : restList
  | otherwise = (x, 1) : rle rest
  where
    (f, l) : restList = rle rest

triples :: [(a, b, c)] -> ([a], [b], [c])
triples [] = ([], [], [])
triples [(x, y, z)] = ([x], [y], [z])
triples ((x, y, z) : rest) = (x : xs, y : ys, z : zs)
  where
    (xs, ys, zs) = triples rest

isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [] _ = ([], [])
isolate (y : ys) x
  | x == y = (l1, x : l2)
  | otherwise = (y : l1, l2)
  where
    (l1, l2) = isolate ys x

odd' x = mod x 2 == 1

amy :: (a -> Bool) -> [a] -> Bool
amy f [] = False
amy f (x : rest)
  | f x = True
  | otherwise = amy f rest

frequencies :: [Char] -> [(Char, Int)]
frequencies [] = []
frequencies (x : rest) = update x freqs
  where
    freqs = frequencies rest
    update x [] = [(x, 1)]
    update x ((v, f) : fs)
      | x == v = (v, f + 1) : fs
      | otherwise = (v, f) : update x fs
