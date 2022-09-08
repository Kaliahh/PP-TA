second :: [a] -> Maybe a
second [] = Nothing
second [x] = Nothing
second (_:y:rest) = Just y

second' :: [a] -> a
second' [] = error "Empty list"
second' [x] = error "No second element"
second' (_:y:_) = y

secondinlist :: [a] -> [a]
secondinlist [] = []
secondinlist [x] = []
secondinlist (_:y:_) = [y]

allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [x] = [x]
allbutsecond (x:_:rest) = x : rest

midtover :: (Num b) => [a] -> b -> ([a], [a])
midtover [] _ = ([], [])
-- midtover [x, y] _ = ([x], [y])
midtover (x:rest) n | n > 0 = ()

-- midtover (x:rest) n = (take (n `div` 2) (x:rest), reverse (take (n `div` 2) (reverse rest)))


-- midtoverHelper 

-- midtoverHelper :: (Num a, Ord a) => [b] -> a -> a -> [a] -> ([a], [a])
-- midtoverHelper (x:rest) len current result | current < len `div` 2 = 


-- midtover' :: [a] -> ([a], [a])
-- midtover' list = midtoverHelper list (length list `div` 2) (length list)

-- midtoverHelper :: (Num b, Ord b, Integral c) => [a] -> c -> b -> ([a], [a])
-- midtoverHelper [] _ _ = ([], [])
-- midtoverHelper [x, y] _ _ = ([x], [y])
-- midtoverHelper (x:rest) half current | current >= half = midtoverHelper rest half (current - 1) 